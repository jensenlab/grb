
test1 <- function() {
  env <- GRBenv$new()
  model <- GRBmodel$new(env, name="testModel")
  .Call("GRB_addvar", model$exptr, 0L, integer(0), numeric(0), 1.0, 0.0, 1.0, 'B', "var1")
  .Call("GRB_addvar", model$exptr, 0L, integer(0), numeric(0), 1.0, 0.0, 1.0, 'B', "var2")
  .Call("GRB_updatemodel", model$exptr)
  .Call("GRB_getstrattrarray", model$exptr, "VarName", 0L, 2L)
  .Call("GRB_addconstr", model$exptr, 2L, as.integer(0:1), c(3.2, -1.4), ">", 0.0, "constr1")
  .Call("GRB_addvar", model$exptr, 0L, integer(0), numeric(0), 1.0, 0.0, 1.0, 'B', "var3")
  .Call("GRB_addgenconstrMax", model$exptr, "maxcon1", 2L, 2L, as.integer(0:1), 0.0)
  .Call("GRB_getattrinfo", model$exptr, "ModelName")
  .Call("GRB_write", model$exptr, "/Users/jensen/Desktop/model.lp")

  return(model)
}

pwl_test <- function() {
  #dyn.load("src/grb.so")

  env <- GRBenv$new()
  model <- GRBmodel$new(env, name="piecewise")
  model$addvars(c("x", "y", "z"), lb=0.0, ub=1.0, vtype='C')

  model$setattr("Obj", c(y=-1.0))
  npts = 101
  ptu = (0:(npts-1)) / (npts-1)
  ptf = exp(-ptu)
  ptg = 2*ptu^2 - 4*ptu
  model$setpwlobj("x", ptu, ptf)
  model$setpwlobj("z", ptu, ptg)

  model$addconstr(values=c(x=1.0, y=2.0, z=3.0), sense="<", rhs=4.0, name="c0")
  model$addconstr(values=c(x=1.0, y=1.0), sense=">", rhs=1.0, name="c1")

  model$optimize()

  print(model$getattr("IsMIP"))
  print(model$getattr("X"))
  print(model$getattr("ObjVal"))
}
