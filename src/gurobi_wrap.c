#include <stdio.h>
#include <Rdefines.h>

#include "gurobi_c.h"

#define RETURN(X)                                              \
  SEXP rval = PROTECT(X);                                      \
  UNPROTECT(1);                                                \
  return rval

#define RETURN_INT(X) RETURN(ScalarInteger(X))
#define RETURN_REAL(X) RETURN(ScalarReal(X))
#define RETURN_STRING(X) RETURN(mkString(X))

#define RETURN_XPTR(X,F)                                                     \
  SEXP return_exptr = PROTECT(R_MakeExternalPtr(X, R_NilValue, R_NilValue)); \
  R_RegisterCFinalizer(return_exptr, F);                                     \
  UNPROTECT(1);                                                              \
  return return_exptr

#define RECAST_ENV(E) (GRBenv*) R_ExternalPtrAddr(E)
#define RECAST_MODEL(M) (GRBmodel*) R_ExternalPtrAddr(M)


// Example of creating a names list:
//    list(name1=1, name2=0.0)
//
//    START_LIST(mylist, 2);
//    ADD_NAMED(mylist, 0, "name1", ScalarInteger(1));
//    ADD_NAMED(mylist, 1, "name2", ScalarReal(0.0));
//    STOP_LIST(mylist);
#define START_LIST(L,N)                                          \
  SEXP L = PROTECT(allocVector(VECSXP, N));                    \
  SEXP _names_L = PROTECT(allocVector(STRSXP, N))
#define ADD_NAMED(L,I,NM,V)                                        \
  SET_VECTOR_ELT(L, I, V);                                           \
  SET_STRING_ELT(_names_L, I, mkChar(NM))
#define STOP_LIST(L)                                           \
  setAttrib(L, R_NamesSymbol, _names_L);                       \
  UNPROTECT(2)

#define NULLABLE(X, CONV) isNull(x) ? NULL : (CONV)


/* =======Environment Creation and Destruction =======*/

// GRBfreeenv wrapper for external pointers
static void _GRBenv_finalizer(SEXP ext_env) {
  GRBfreeenv(RECAST_ENV(ext_env));
}

// GRBloadenv
SEXP GRB_loadenv(SEXP logfilename) {
  GRBenv *env = NULL;
  int error = GRBloadenv(
    &env,
    CHAR(asChar(logfilename))[0]
  );
  if (error) Rprintf("Error loading GRBenv: %i\n", error);

  RETURN_XPTR(env, _GRBenv_finalizer);
}

// GRBloadclientenv

// GRBloadcloudenv

// GRBfreeenv
void GRB_freeenv(SEXP ext_env) {
  GRBfreeenv(RECAST_ENV(ext_env));
}

// GRBgetconcurrentenv

// GRBdiscardconcurrentenvs


/* =======Model Creation and Modification =======*/

// GRBfreemodel wrapped for external pointers
static void _GRBmodel_finalizer(SEXP ext_model) {
  GRBfreemodel(RECAST_MODEL(ext_model));
}

// GRBloadmodel

// GRBnewmodel
// TODO: expand to allow defaults
SEXP GRB_newmodel(SEXP ext_env,  SEXP modelname) {
  GRBenv *env = RECAST_ENV(ext_env);
  GRBmodel *model = NULL;
  const char *name = CHAR(asChar(modelname));
  int error = GRBnewmodel(
    env,
    &model,
    name,
    0,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
  );
  if (error) Rprintf("Error loading GRBmodel: %i\n", error);

  RETURN_XPTR(model, _GRBmodel_finalizer);
}

// GRBcopymodel
SEXP GRB_copymodel(SEXP ext_model) {
  GRBmodel *copy = GRBcopymodel(RECAST_MODEL(ext_model));
  RETURN_XPTR(copy, _GRBmodel_finalizer);
}

// GRBaddconstr
SEXP GRB_addconstr(SEXP ext_model,  // GRBmodel*
                   SEXP numnz,      // int
                   SEXP cind,       // int*
                   SEXP cval,       // double*
                   SEXP sense,      // char
                   SEXP rhs,        // double
                   SEXP constrname  // const char*
                  ) {
  int error = GRBaddconstr(
    RECAST_MODEL(ext_model),
    INTEGER(numnz)[0],
    INTEGER(cind),
    REAL(cval),
    CHAR(asChar(sense))[0],
    REAL(rhs)[0],
    CHAR(asChar(constrname))
  );

  RETURN_INT(error);
}

// GRBaddconstrs

// GRBaddgenconstrMax
SEXP GRB_addgenconstrMax(SEXP ext_model,
                         SEXP name,
                         SEXP resvar,
                         SEXP nvars,
                         SEXP vars,
                         SEXP constant) {
  int error = GRBaddgenconstrMax(
    RECAST_MODEL(ext_model),
    CHAR(asChar(name)),
    INTEGER(resvar)[0],
    INTEGER(nvars)[0],
    INTEGER(vars),
    REAL(constant)[0]
  );

  RETURN_INT(error);
}

// GRBaddgenconstrMin
SEXP GRB_addgenconstrMin(SEXP ext_model,
                         SEXP name,
                         SEXP resvar,
                         SEXP nvars,
                         SEXP vars,
                         SEXP constant) {
  int error = GRBaddgenconstrMin(
    RECAST_MODEL(ext_model),
    CHAR(asChar(name)),
    INTEGER(resvar)[0],
    INTEGER(nvars)[0],
    INTEGER(vars),
    REAL(constant)[0]
  );

  RETURN_INT(error);
}

// GRBaddgenconstrAbs
SEXP GRB_addgenconstrAbs(SEXP ext_model,
                         SEXP name,
                         SEXP resvar,
                         SEXP argvar) {
  int error = GRBaddgenconstrAbs(
    RECAST_MODEL(ext_model),
    CHAR(asChar(name)),
    INTEGER(resvar)[0],
    INTEGER(argvar)[0]
  );

  RETURN_INT(error);
}

// GRBaddgenconstrAnd
SEXP GRB_addgenconstrAnd(SEXP ext_model,
                         SEXP name,
                         SEXP resvar,
                         SEXP nvars,
                         SEXP vars) {
  int error = GRBaddgenconstrAnd(
    RECAST_MODEL(ext_model),
    CHAR(asChar(name)),
    INTEGER(resvar)[0],
    INTEGER(nvars)[0],
    INTEGER(vars)
  );

  RETURN_INT(error);
}

// GRBaddgenconstrOr
SEXP GRB_addgenconstrOr(SEXP ext_model,
                        SEXP name,
                        SEXP resvar,
                        SEXP nvars,
                        SEXP vars) {
  int error = GRBaddgenconstrOr(
    RECAST_MODEL(ext_model),
    CHAR(asChar(name)),
    INTEGER(resvar)[0],
    INTEGER(nvars)[0],
    INTEGER(vars)
  );

  RETURN_INT(error);
}

// GRBaddgenconstrIndicator
SEXP GRB_addgenconstrIndicator(SEXP ext_model,
                               SEXP name,
                               SEXP binvar,
                               SEXP binval,
                               SEXP nvars,
                               SEXP ind,
                               SEXP val,
                               SEXP sense,
                               SEXP rhs) {
  int error = GRBaddgenconstrIndicator(
    RECAST_MODEL(ext_model),
    CHAR(asChar(name)),
    INTEGER(binvar)[0],
    INTEGER(binval)[0],
    INTEGER(nvars)[0],
    INTEGER(ind),
    REAL(val),
    CHAR(asChar(sense))[0],
    REAL(rhs)[0]
  );

  RETURN_INT(error);
}

// GRBaddqconstr

// GRBaddqpterms

// GRBaddrangeconstr
SEXP GRB_addrangeconstr(SEXP ext_model,
                        SEXP numnz,
                        SEXP cind,
                        SEXP cval,
                        SEXP lower,
                        SEXP upper,
                        SEXP constrname) {
  int error = GRBaddrangeconstr(
    RECAST_MODEL(ext_model),
    INTEGER(numnz)[0],
    INTEGER(cind),
    REAL(cval),
    REAL(lower)[0],
    REAL(upper)[0],
    CHAR(asChar(constrname))
  );

  RETURN_INT(error);
}

// GRBaddrangeconstrs

// GRBaddsos
SEXP GRB_addsos(SEXP ext_model,
                SEXP numsos,
                SEXP nummembers,
                SEXP types,
                SEXP beg,
                SEXP ind,
                SEXP weight) {
  int error = GRBaddsos(
    RECAST_MODEL(ext_model),
    INTEGER(numsos)[0],
    INTEGER(nummembers)[0],
    INTEGER(types),
    INTEGER(beg),
    INTEGER(ind),
    REAL(weight)
  );

  RETURN_INT(error);
}

// GRBaddvar
SEXP GRB_addvar(SEXP ext_model,   // GRBmodel*
                SEXP numnz,       // int
                SEXP vind,        // int*
                SEXP vval,        // double*
                SEXP obj,         // double
                SEXP lb,          // double
                SEXP ub,          // double
                SEXP vtype,       // char
                SEXP varname      // const char*
               ) {
  int error = GRBaddvar(
    RECAST_MODEL(ext_model),
    INTEGER(numnz)[0],
    INTEGER(vind),
    REAL(vval),
    REAL(obj)[0],
    REAL(lb)[0],
    REAL(ub)[0],
    CHAR(asChar(vtype))[0],
    CHAR(asChar(varname))
  );

  RETURN_INT(error);
}

// GRBaddvars
SEXP GRB_addvars(SEXP ext_model,
                 SEXP numvars,
                 SEXP numnz,
                 SEXP vbeg,
                 SEXP vind,
                 SEXP vval,
                 SEXP obj,
                 SEXP ub,
                 SEXP lb,
                 SEXP vtype,
                 SEXP varnames) {
  int error = GRBaddvars(
    RECAST_MODEL(ext_model),
    INTEGER(numvars)[0],
    INTEGER(numnz)[0],
    INTEGER(vbeg),
    INTEGER(vind),
    REAL(vval),
    REAL(obj),
    REAL(lb),
    REAL(ub),
    CHAR(asChar(vtype))[0],
    CHAR(asChar(varnames))
  );

  RETURN_INT(error);
}

// GRBchgcoeffs
SEXP GRB_chgcoeffs(SEXP ext_model,
                   SEXP numchgs,
                   SEXP cind,
                   SEXP vind,
                   SEXP val) {
  int error = GRBchgcoeffs(
    RECAST_MODEL(ext_model),
    INTEGER(numchgs)[0],
    INTEGER(cind),
    INTEGER(vind),
    REAL(val)
  );

  RETURN_INT(error);
}

// GRBdelconstrs
SEXP GRB_delconstrs(SEXP ext_model,
                    SEXP numdel,
                    SEXP ind) {
  int error = GRBdelconstrs(
    RECAST_MODEL(ext_model),
    INTEGER(numdel)[0],
    INTEGER(ind)
  );

  RETURN_INT(error);
}

// GRBdelgenconstrs
SEXP GRB_delgenconstrs(SEXP ext_model,
                       SEXP numdel,
                       SEXP ind) {
  int error = GRBdelgenconstrs(
    RECAST_MODEL(ext_model),
    INTEGER(numdel)[0],
    INTEGER(ind)
  );

  RETURN_INT(error);
}

// GRBdelq
SEXP GRB_delq(SEXP ext_model) {
  int error = GRBdelq(
    RECAST_MODEL(ext_model)
  );

  RETURN_INT(error);
}

// GRBdelqconstrs
SEXP GRB_delqconstrs(SEXP ext_model,
                     SEXP numdel,
                     SEXP ind) {
  int error = GRBdelqconstrs(
    RECAST_MODEL(ext_model),
    INTEGER(numdel)[0],
    INTEGER(ind)
  );

  RETURN_INT(error);
}

// GRBdelsos
SEXP GRB_delsos(SEXP ext_model,
                SEXP numdel,
                SEXP ind) {
  int error = GRBdelsos(
    RECAST_MODEL(ext_model),
    INTEGER(numdel)[0],
    INTEGER(ind)
  );

  RETURN_INT(error);
}

// GRBdelvars
SEXP GRB_delvars(SEXP ext_model,
                 SEXP numdel,
                 SEXP ind) {
  int error = GRBdelvars(
    RECAST_MODEL(ext_model),
    INTEGER(numdel)[0],
    INTEGER(ind)
  );

  RETURN_INT(error);
}

// GRBsetpwlobj
SEXP GRB_setpwlobj(SEXP ext_model,
                   SEXP var,
                   SEXP npoints,
                   SEXP x,
                   SEXP y) {
  int error = GRBsetpwlobj(
    RECAST_MODEL(ext_model),
    INTEGER(var)[0],
    INTEGER(npoints)[0],
    REAL(x),
    REAL(y)
  );

  RETURN_INT(error);
}

// GRBupdatemodel
SEXP GRB_updatemodel(SEXP ext_model) {
  int error = GRBupdatemodel(RECAST_MODEL(ext_model));
  RETURN_INT(error);
}

// GRBfreemodel
void GRB_freemodel(SEXP ext_model) {
  GRBfreemodel(RECAST_MODEL(ext_model));
}

// GRBXaddconstrs

// GRBXaddrangeconstrs

// GRBXaddvars

// GRBXchgcoeffs

// GRBXloadmodel


/* =======Model Solution =======*/

// GRBoptimize
SEXP GRB_optimize(SEXP ext_model) {
  int error = GRBoptimize(RECAST_MODEL(ext_model));
  RETURN_INT(error);
}

// GRBoptimizeasync
SEXP GRB_optimizeasync(SEXP ext_model) {
  int error = GRBoptimizeasync(RECAST_MODEL(ext_model));
  RETURN_INT(error);
}

// GRBcomputeIIS
SEXP GRB_computeIIS(SEXP ext_model) {
  int error = GRBcomputeIIS(RECAST_MODEL(ext_model));
  RETURN_INT(error);
}

// GRBfeasrelax

// GRBfixedmodel
SEXP GRB_fixedmodel(SEXP ext_model) {
  GRBmodel *fixed = GRBfixedmodel(RECAST_MODEL(ext_model));
  RETURN_XPTR(fixed, _GRBmodel_finalizer);
}

// GRBresetmodel
SEXP GRB_resetmodel(SEXP ext_model) {
  int error = GRBresetmodel(RECAST_MODEL(ext_model));
  RETURN_INT(error);
}

// GRBsync
SEXP GRB_sync(SEXP ext_model) {
  int error = GRBsync(RECAST_MODEL(ext_model));
  RETURN_INT(error);
}


/* =======Model Queries =======*/

/* =======Input/Output =======*/

// GRBreadmodel
SEXP GRB_readmodel(SEXP ext_env,    // GRBenv*
                   SEXP filename   // const char*
                  ) {
  GRBmodel* model = NULL;
  int error = GRBreadmodel(
    RECAST_ENV(ext_env),
    CHAR(asChar(filename)),
    &model
  );
  RETURN_XPTR(model, _GRBmodel_finalizer);
}

// GRBread
SEXP GRB_read(SEXP ext_model,  // GRBmodel*
              SEXP filename    // const char*
             ) {
  int error = GRBread(
    RECAST_MODEL(ext_model),
    CHAR(asChar(filename))
  );
  RETURN_INT(error);
}

// GRBwrite
SEXP GRB_write(SEXP ext_model,  // GRBmodel*
               SEXP filename    // const char*
              ) {
  int error = GRBwrite(
    RECAST_MODEL(ext_model),
    CHAR(asChar(filename))
  );
  RETURN_INT(error);
}


/* =======Attribute Management =======*/

// GRBgetattrinfo
SEXP GRB_getattrinfo(SEXP ext_model,   // GRBmodel*
                     SEXP attrname     // const char*
                     ) {
  int datatype, attrtype, settable;
  int error = GRBgetattrinfo(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    &datatype,
    &attrtype,
    &settable
  );

  START_LIST(alist, 3);
  ADD_NAMED(alist, 0, "datatype", ScalarInteger(datatype));
  ADD_NAMED(alist, 1, "attrtype", ScalarInteger(attrtype));
  ADD_NAMED(alist, 2, "settable", ScalarInteger(settable));
  STOP_LIST(alist);

  return(alist);
}

// GRBgetintattr
SEXP GRB_getintattr(SEXP ext_model,
                    SEXP attrname) {
  int result;
  int error = GRBgetintattr(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    &result
  );
  RETURN_INT(result);
}

// GRBsetintattr
SEXP GRB_setintattr(SEXP ext_model,
                    SEXP attrname,
                    SEXP newvalue) {
  int error = GRBsetintattr(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(newvalue)[0]
  );
  RETURN_INT(error);
}

// GRBgetintattrelement
SEXP GRB_getintattrelement(SEXP ext_model,
                           SEXP attrname,
                           SEXP element) {
  int result;
  int error = GRBgetintattrelement(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(element)[0],
    &result
  );
  RETURN_INT(result);
}

// GRBsetintattrelement
SEXP GRB_setintattrelement(SEXP ext_model,
                           SEXP attrname,
                           SEXP element,
                           SEXP newvalue) {
  int error = GRBsetintattrelement(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(element)[0],
    INTEGER(newvalue)[0]
  );
  RETURN_INT(error);
}

// GRBgetintattrarray
SEXP GRB_getintattrarray(SEXP ext_model,
                         SEXP attrname,
                         SEXP start,
                         SEXP len) {
  int length = INTEGER(len)[0];
  SEXP values = PROTECT(allocVector(INTSXP, length));
  int error = GRBgetintattrarray(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(start)[0],
    length,
    INTEGER(values)
  );
  UNPROTECT(1);

  return values;
}

// GRBsetintattrarray
SEXP GRB_setintattrarray(SEXP ext_model,
                         SEXP attrname,
                         SEXP start,
                         SEXP len,
                         SEXP values) {
  int error = GRBsetintattrarray(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(start)[0],
    INTEGER(len)[0],
    INTEGER(values)
  );

  RETURN_INT(error);
}

// GRBgetintattrlist
SEXP GRB_getintattrlist(SEXP ext_model,
                        SEXP attrname,
                        SEXP len,
                        SEXP ind) {
  int length = INTEGER(len)[0];
  SEXP values = PROTECT(allocVector(INTSXP, length));
  int error = GRBgetintattrlist(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    length,
    INTEGER(ind),
    INTEGER(values)
  );
  UNPROTECT(1);

  return values;
}

// GRBsetintattrlist
SEXP GRB_setintattrlist(SEXP ext_model,
                        SEXP attrname,
                        SEXP len,
                        SEXP ind,
                        SEXP values) {
  int error = GRBsetintattrlist(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(len)[0],
    INTEGER(ind),
    INTEGER(values)
  );

  RETURN_INT(error);
}



// GRBgetdblattr
SEXP GRB_getdblattr(SEXP ext_model,
                    SEXP attrname) {
  double result;
  int error = GRBgetdblattr(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    &result
  );
  RETURN_REAL(result);
}

// GRBsetdblattr

// GRBgetdblattrelement

// GRBsetdblattrelement

// GRBgetdblattrarray
SEXP GRB_getdblattrarray(SEXP ext_model,
                         SEXP attrname,
                         SEXP start,
                         SEXP len) {
  int length = INTEGER(len)[0];
  SEXP values = PROTECT(allocVector(REALSXP, length));
  int error = GRBgetdblattrarray(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(start)[0],
    length,
    REAL(values)
  );
  UNPROTECT(1);

  return values;
}

// GRBsetdblattrarray

// GRBgetdblattrlist

// GRBsetdblattrlist
SEXP GRB_setdblattrlist(SEXP ext_model,
                        SEXP attrname,
                        SEXP len,
                        SEXP ind,
                        SEXP values) {
  int error = GRBsetdblattrlist(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(len)[0],
    INTEGER(ind),
    REAL(values)
  );

  RETURN_INT(error);
}


// GRBgetcharattr

// GRBsetcharattr

// GRBgetcharattrelement

// GRBsetcharattrelement

// GRBgetcharattrarray

// GRBsetcharattrarray

// GRBgetcharattrlist

// GRBsetcharattrlist
SEXP GRB_setcharattrlist(SEXP ext_model,
                         SEXP attrname,
                         SEXP len,
                         SEXP ind,
                         SEXP values) {
  int error = GRBsetcharattrlist(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(len)[0],
    INTEGER(ind),
    CHAR(asChar(values))
  );

  RETURN_INT(error);
}


// GRBgetstrattr
SEXP GRB_getstrattr(SEXP ext_model,
                    SEXP attrname) {
  char *value;
  GRBgetstrattr(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    &value
  );

  RETURN_STRING(value);
}

// GRBsetstrattr

// GRBgetstrattrelement

// GRBsetstrattrelement

// GRBgetstrattrarray
SEXP GRB_getstrattrarray(SEXP ext_model,
                         SEXP attrname,
                         SEXP start,
                         SEXP len) {
  int length = INTEGER(len)[0];
  char *raw_values[length];
  int error = GRBgetstrattrarray(
    RECAST_MODEL(ext_model),
    CHAR(asChar(attrname)),
    INTEGER(start)[0],
    length,
    raw_values
  );
  SEXP values = PROTECT(allocVector(STRSXP, length));
  for (int i=0; i<length; i++) {
    SET_STRING_ELT(values, i, mkChar(raw_values[i]));
  }
  UNPROTECT(1);
  return(values);
}

// GRBsetstrattrarray

// GRBgetstrattrlist

// GRBsetstrattrlist


/* =======Parameter Management and Tuning =======*/

/* =======Monitoring Progress =======*/

/* =======Modifying Solver Behavior =======*/

/* =======Error Handling =======*/

/* =======Advanced Simplex Routines =======*/
