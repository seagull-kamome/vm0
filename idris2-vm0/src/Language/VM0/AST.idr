--
-- Definitions of VM0 virtual machine AST
--
-- GLOSSARY:
--   arg : formal parameter
--   param : actual parameter
--
module Language.VM0.AST

import Control.Monad.RWS
import Control.Monad.State

import Compiler.Common
import Compiler.CompileExpr
import Compiler.LambdaLift

import Data.Vect
import Data.List
import Data.List1
import Data.SortedMap
import Core.Context -- Ref
import Core.Name   -- Name
import Core.FC     -- FC
import Core.TT     -- LazyReason, IsVar
import Core.CompileExpr -- ConInfo

%default total


-- --------------------------------------------------------------------------


public export data BitWidth = BWDefault | BW64 | BW32 | BW16 | BW8
public export
Eq BitWidth where
  BWDefault == BWDefault = True
  BW64 == BW64 = True
  BW32 == BW32 = True
  BW16 == BW16 = True
  BW8  == BW8  = True
  _ == _ = False

public export
Show BitWidth where
  show = \case
    BWDefault => ""
    BW64 => "64"
    BW32 => "32" 
    BW16 => "16"
    BW8  => "8"

public export
data IsShortBW : BitWidth -> Type where
  ItsBW32 : (0 x:BitWidth) -> {0 _:x=BW32} -> IsShortBW x
  ItsBW16 : (0 x:BitWidth) -> {0 _:x=BW16} -> IsShortBW x
  ItsBW8  : (0 x:BitWidth) -> {0 _:x=BW8}  -> IsShortBW x

data Ty : Type
Eq Ty
Show Ty

public export
data ObjTy : Type where
  ObjInt     : BitWidth -> ObjTy
  ObjUInt    : BitWidth -> ObjTy
  ObjDouble  : ObjTy
  ObjFloat   : ObjTy
  ObjInteger : ObjTy
  ObjObj     : ObjTy
  ObjData    : ObjTy
  ObjClosure : Ty -> List Ty -> ObjTy
  ObjBuffer  : ObjTy
  ObjGCPtr   : ObjTy

covering
public export
Eq ObjTy where
  ObjInt bw == ObjInt bw' = bw == bw'
  ObjUInt bw == ObjUInt bw' = bw == bw'
  ObjDouble == ObjDouble = True
  ObjFloat == ObjFloat = True
  ObjInteger == ObjInteger = True
  ObjData == ObjData = True
  ObjClosure ty params == ObjClosure ty' params' =
    (ty == ty') && (params == params')
  ObjBuffer == ObjBuffer = True
  ObjGCPtr == ObjGCPtr = True
  _ == _ = False


public export toCTypSfx : ObjTy -> String
toCTypSfx = \case
  ObjInt w => "int\{show w}"
  ObjUInt w => "uint\{show w}"
  ObjDouble => "double"
  ObjFloat => "float"
  ObjInteger => "integer"
  ObjObj => "objarray"
  ObjData => "data"
  ObjClosure _ _ => "closure"
  ObjBuffer => "buffer"
  ObjGCPtr => "gcptr"

covering
public export
Show ObjTy where
  show = \case
    ObjInt w => "int\{show w}"
    ObjUInt w => "uint\{show w}"
    ObjDouble => "double"
    ObjFloat => "float"
    ObjInteger => "integer"
    ObjObj => "ObArray"
    ObjData => "Data"
    ObjClosure ty params => "Closure(\{concat $ intersperse " -> " (map show params)} -> \{show ty}"
    ObjBuffer => "Buffer"
    ObjGCPtr => "GCPtr"


public export toCTyp : ObjTy -> String
toCTyp x = "struct vm0_obj_\{toCTypSfx x}"

public export toCObjTyID : ObjTy -> String
toCObjTyID x = case x of
  ObjInt w => "VM0_OBJ_TYPE_INT\{show w}"
  ObjUInt w => "VM0_OBJ_TYPE_UINT\{show w}"
  ObjDouble => "VM0_OBJ_TYPE_DOUBLE"
  ObjFloat => "VM0_OBJ_TYPE_FLOAT"
  ObjInteger => "VM0_OBJ_TYPE_INTEGER"
  ObjObj => "VM0_OBJ_TYPE_OBJ"
  ObjData => "VM0_OBJ_TYPE_DATA"
  ObjClosure _ _ => "VM0_OBJ_TYPE_CLOSURE"
  ObjBuffer => "VM0_OBJ_TYPE_BUFFER"
  ObjGCPtr => "VM0_OBJ_TYPE_GC_PTR"
  

public export
data Ty : Type where
  TyVar : Nat -> Ty
  --
  TyVoid : Ty           -- "void"
  TyValue : Ty          -- "vm0_value_t"
  TyAnyObj : Ty         -- it's objet but wrapped into "vm0_value_t"
  TyObj : ObjTy -> Ty   -- it's objet but wrapped into "vm0_value_t"
  TyUnboxedInt : (bw:BitWidth) -> {auto 0 _:IsShortBW bw} -> Ty
  TyUnboxedUInt : (bw:BitWidth) -> {auto 0 _:IsShortBW bw} -> Ty
  TyUnboxedFloat : Ty
  TyCInt : BitWidth -> Ty   -- "intN_t"
  TyCUInt : BitWidth -> Ty  -- "uintN_t"
  TyCDouble : Ty            -- "double"
  TyCFloat : Ty             -- "float"
  TyCAnyObj: Ty             -- "struct vm0_obj_hdr *"
  TyCObj : ObjTy -> Ty      -- "struct vm0_obj_??? *"
  TyFunction : Ty
  TyConstructor : Ty

covering
public export
Eq Ty where
  TyVar n == TyVar m   = n == m
  TyVoid  == TyVoid    = True
  TyValue == TyValue   = True
  TyAnyObj == TyAnyObj = True
  TyUnboxedInt bw == TyUnboxedInt bw'   = bw == bw'
  TyUnboxedUInt bw == TyUnboxedUInt bw' = bw == bw'
  TyUnboxedFloat == TyUnboxedFloat      = True
  TyCInt bw == TyCInt bw'   = bw == bw'
  TyCUInt bw == TyCUInt bw' = bw == bw'
  TyCDouble == TyCDouble    = True
  TyCFloat == TyCFloat      = True
  TyCAnyObj == TyCAnyObj    = True
  TyCObj lty == TyCObj rty  = lty == rty
  TyFunction == TyFunction  = True
  TyConstructor == TyConstructor = True
  _ == _ = False

covering
public export
Show Ty where
  show = \case
    TyVar n => "T\{show n}]"
    TyVoid => "void"
    TyValue => "any"
    TyAnyObj => "obj"
    TyObj ty => "obj_\{show ty}"
    TyUnboxedInt w => "int\{show w}"
    TyUnboxedUInt w => "uint\{show w}"
    TyUnboxedFloat => "float"
    TyCInt w => "cint\{show w}"
    TyCUInt w => "cuint\{show w}"
    TyCDouble => "cdouble"
    TyCFloat => "cfloat"
    TyCAnyObj => "canyobj"
    TyCObj objty => "obj_\{show objty}"
    TyFunction => "function"
    TyConstructor => "constr"


-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------

-- scope variables
public export
data IsVar : Name -> Nat -> Ty -> List (Ty, Name) -> Type where
  First : IsVar nm Z ty ((ty,nm)::xs)
  Later : IsVar nm i ty xs -> IsVar nm (S i) ty (_::xs)
  
lookupVar : (i:Nat) -> (vars:List (Ty, Name))
         -> Maybe (ty:Ty ** n:Name ** IsVar n i ty vars)
lookupVar _ [] = Nothing
lookupVar Z ((ty,n)::_) = Just (ty ** n ** First)
lookupVar (S i) (_::xs) with (lookupVar i xs)
  _ | Nothing = Nothing
  _ | Just (ty ** n ** p) = Just (ty ** n ** Later p)


-- --------------------------------------------------------------------------
{- 
public export
data PrimOp : (arity:Nat) -> Type where
  -- numeric operations.
  OpAdd    : PrimOp 2
  OpSub    : PrimOp 2
  OpMul    : PrimOp 2
  OpDiv    : PrimOp 2
  OpMod    : PrimOp 2
  OpNeg    : PrimOp 1
  OpShiftL : PrimOp 2
  OpSHiftR : PrimOp 2
  OpRotL   : PrimOp 2
  OpRotR   : PrimOp 2
  OpBitAnd : PrimOp 2
  OpBitOr  : PrimOp 2
  OpBitXor : PrimOp 2
  -- comparator
  OpLT     : PrimOp 2
  OpLE     : PrimOp 2
  OpGT     : PrimOp 2
  OpGE     : PrimOp 2
  OpEQ     : PrimOp 2
  OpNE     : PrimOp 2
  -- string operation
  OpStrLen  : PrimOp 1
  OpStrHead : PrimOp 1
  OpStrTail : PrimOp 1
  OpStrIndex : PrimOp 2
  OpStrCons : PrimOp 2
  OpStrAppend : PrimOp 2
  OpStrReverse : PrimOp 1
  OpStrSubstr : PrimOp 3
  -- floating point operation
  OpExp  : PrimOp 1
  OpLog  : PrimOp 1
  OpPow  : PrimOp 2
  OpSin  : PrimOp 1
  OpCos  : PrimOp 1
  OpTan  : PrimOp 1
  OpASin : PrimOp 1
  OpACos : PrimOp 1
  OpATan : PrimOp 1
  OpSqrt : PrimOp 1
  OpFloor : PrimOp 1
  OpCailing : PrimOp 1
  -- Cast
  OpCast      : PrimOp 1
  OpBelieveMe : PrimOp 3
  OpCrash     : PrimOp 2

-}

-- --------------------------------------------------------------------------

public export
Env : Type
Env = List (Ty, Name)


public export
data ConAlt : (f:Env -> Ty -> Type) -> (vars:Env) -> Type where
  MkConAlt : Name -> (info:ConInfo) -> (tag:Maybe Int)
           -> (args:Env)
           -> (ty:Ty) -> f (params ++ vars) ty
           -> ConAlt f vars


public export
data Lifted : Env -> Ty -> Type where
  LLocal   : FC -> (idx:Nat) -> (ty:Ty) -> (0 prf:IsVar _ idx ty vars) -> Lifted vars ty
  LAppName : FC -> Maybe LazyReason -> Name -> (retTy:Ty)
          -> List (ty:Ty ** Lifted vars ty)
          -> Lifted vars ty
  LApp     : FC -> Maybe LazyReason
          -> (retTy:Ty)
          -> (paramTy:List Ty)
          -> Lifted vars (TyObj (ObjClosure retTy paramTy))
          -> List (ty:Ty ** Lifted vars ty)
          -> Lifted vars (TyObj (ObjClosure retTy rest))
  LLet     : FC
          -> (name:Name) -> (ty:Ty) -> (expr: Lifted vars ty)
          -> (body:Lifted ((ty, name)::vars) bodyTy) -> Lifted vars bodyTy
  LCon     : FC
          -> Name -> Maybe Int
          -> List (ty:Ty ** Lifted vars ty)
          -> Lifted vars (TyCObj ObjData)
  LOp      : FC -> Maybe LazyReason
          -> {0 arity:Nat} -> PrimFn arity
          -> (retTy:Ty)
          -> Vect arity (ty:Ty ** Lifted vars ty)
          -> Lifted vars retTy
  LExtPrim : FC -> Maybe LazyReason
          -> (p : Name) -> (retTy:Ty)
          -> List (ty:Ty ** Lifted vars ty)
          -> Lifted vars retTy
  LConCase : FC
          -> (retTy:Ty)
          -> (expr:Lifted vars ty)
          -> (alts:List (ConAlt Lifted vars) )
          -> (dft:Maybe (ty:Ty ** Lifted vars ty))
          -> Lifted vars retTy
  LConstCase : FC
          -> (retTy:Ty)
          -> {0 ty:Ty} -> (expr:Lifted vars ty)
          -> (alts:List (Constant, (ty':Ty ** Lifted vars ty')))
          -> (def : Maybe (ty':Ty ** Lifted vars ty')) -> Lifted vars retTy
  LPrimVal : FC -> Constant -> Lifted vars ty
  LErased  : FC -> Lifted vars ty
  LCrash   : FC -> (msg : String) -> Lifted varss ty


public export
record ConstrDcl where
  constructor MkConstrDcl
  dclname : Name
  args : List Ty

public export
record FuncDef where
  constructor MkFuncDef
  dclname : Name
  params : List (Ty, Name)
  resultTy : Ty
  resultMayClosure : Bool
  body : Lifted params resultTy

public export
record FFIDcl where
  constructor MkFFIDcl
  dclname : Name
  ccs : List String
  params : List CFType
  resultTy : CFType



-- --------------------------------------------------------------------------

fatal : FC -> String -> a
fatal fc msg = assert_total $ idris_crash "\{show fc} VM0 codegen: \{msg}"


-- --------------------------------------------------------------------------

record TyEnv where
  constructor MkTyEnv
  nextVar : Nat
  {0 lenv, renv:List (Ty,Name)}
  assignments : List ((lty:Ty ** Lifted lenv lty), (rty:Ty ** Lifted renv rty))
  resolved : SortedMap Nat Ty

freshTyVar : {auto x:Ref TyEnv TyEnv} -> Core Ty
freshTyVar = do
  x <- get TyEnv
  put TyEnv $ { nextVar := S x.nextVar } x
  pure $ TyVar x.nextVar

covering
assignTy : {auto x:Ref TyEnv TyEnv}
        -> FC
        -> (formal:(lty:Ty ** Lifted _ lty))
        -> (actual:(rty:Ty ** Lifted _ rty))
        -> Core ()
assignTy fc formal actual = ?lhs_assignTy

covering
assignArgsTy : {auto _:Ref TyEnv TyEnv}
            -> FC
            -> List (Ty, Name)
            -> List (ty:Ty ** Lifted _ ty)
            -> Core (List (Ty, Name))
assignArgsTy fc [] []     = pure []
assignArgsTy fc [] (_::_) = fatal fc "assignArgsTy:args too large than params."
assignArgsTy fc ((x, _)::xs) (y::ys) = do
  assignTy fc (x ** LErased EmptyFC) y
  assignArgsTy fc xs ys
assignArgsTy fx rest []   = pure rest




-- --------------------------------------------------------------------------

fromPrimType : PrimType -> Ty
fromPrimType = \case
  IntType => TyCInt BWDefault
  Int8Type => TyCInt BW8
  Int16Type => TyCInt BW16
  Int32Type => TyCInt BW32
  Int64Type => TyCInt BW64
  IntegerType => TyObj ObjInteger
  Bits8Type => TyCUInt BW8
  Bits16Type => TyCUInt BW16
  Bits32Type => TyCUInt BW32
  Bits64Type => TyCUInt BW64
  StringType => TyCObj (ObjUInt BW8)
  CharType => TyCUInt BW8
  DoubleType => TyCDouble
  WorldType => TyVoid

opRetTy : {arity:Nat} -> PrimFn arity -> Maybe Ty
opRetTy = \case 
  Add ty => pure $ fromPrimType ty
  Sub ty => pure $ fromPrimType ty
  Mul ty => pure $ fromPrimType ty
  Div ty => pure $ fromPrimType ty
  Mod ty => pure $ fromPrimType ty
  Neg ty => pure $ fromPrimType ty
  ShiftL ty => pure $ fromPrimType ty
  ShiftR ty => pure $ fromPrimType ty
  BAnd ty => pure $ fromPrimType ty
  BOr ty => pure $ fromPrimType ty
  BXOr ty => pure $ fromPrimType ty
  LT ty => pure $ fromPrimType ty
  LTE ty => pure $ fromPrimType ty
  EQ ty => pure $ fromPrimType ty
  GTE ty => pure $ fromPrimType ty
  GT ty => pure $ fromPrimType ty
  StrLength => pure $ TyCInt BWDefault
  StrHead   => pure $ TyCObj (ObjUInt BW8)
  StrTail   => pure $ TyCObj (ObjUInt BW8)
  StrIndex  => pure $ TyCInt BWDefault
  StrCons   => pure $ TyCObj (ObjUInt BW8)
  StrAppend => pure $ TyCObj (ObjUInt BW8)
  StrReverse => pure $ TyCObj (ObjUInt BW8)
  StrSubstr => pure $ TyCObj (ObjUInt BW8)
  DoubleExp => pure TyCDouble
  DoubleLog => pure TyCDouble
  DoublePow => pure TyCDouble
  DoubleSin => pure TyCDouble
  DoubleCos => pure TyCDouble
  DoubleTan => pure TyCDouble
  DoubleASin => pure TyCDouble
  DoubleACos => pure TyCDouble
  DoubleATan => pure TyCDouble
  DoubleSqrt => pure TyCDouble
  DoubleFloor => pure TyCDouble
  DoubleCeiling => pure TyCDouble
  Cast lty rty => pure $ fromPrimType rty
  BelieveMe => Nothing
  Crash     => Nothing

constType : Constant => Ty
constType = \case
  I _      => TyCInt BWDefault
  I8 _     => TyCInt BW8
  I16 _    => TyCInt BW16
  I32 _    => TyCInt BW32
  I64 _    => TyCInt BW64
  BI _     => TyCUInt BWDefault
  B8 _     => TyCUInt BW8
  B16 _    => TyCUInt BW16
  B32 _    => TyCUInt BW32
  B64 _    => TyCUInt BW64
  Str _    => TyObj (ObjUInt Bw8)
  Ch _     => TyCUInt BW8
  Db _     => TyCDouble
  WorldVal => TyVoid
  Prt      => TyVoid
  



-- --------------------------------------------------------------------------
data ConstrDcls : Type where
data FuncDefs : Type where
data ErrConds : Type where
data FFIDcls : Type where

lookupFuncDef : {auto _:Ref FuncDefs (List FuncDef)}
             -> FC -> Name -> Core FuncDef
lookupFuncDef fc nm = do
  xs <- get FuncDefs
  let Just x = find (\x => x.dclname == nm) xs
        | Nothing => fatal fc "lookupFuncSignature : \{show nm}"
  pure x -- (x.resultTy, x.params)

lookupConstrDcl : {auto _:Ref ConstrDcls (List ConstrDcl)}
               -> FC -> Name -> Core ConstrDcl
lookupConstrDcl fc nm = do
  xs <- get ConstrDcls
  let Just x = find (\x => x.dclname == nm) xs
       | Nothing => fatal fc "lookupConstrDcl : \{show nm}"
  pure x


-- --------------------------------------------------------------------------

covering translateFunction
   : {auto _:Ref TyEnv TyEnv}
  -> {auto _:Ref FuncDefs (List FuncDef)}
  -> {auto _:Ref ConstrDcls (List ConstrDcl)}
  -> {auto _:Ref ErrConds (List (Name, Lifted [] TyVoid))}
  -> {auto _:Ref FFIDcls (List FFIDcl)}
  -> Name
  -> {0 vars:List Name} -> Compiler.LambdaLift.Lifted vars
  -> Core ()
translateFunction {vars=vars} dclname expr = do
  let (retTy, params) = lookupFuncSignature EmptyFC dclname
  r@(retTy' ** newexpr) <- go expr params
  assignTy EmptyFC (retTy ** Erased EmptyFC) r
  --
  xs <- get FuncDefs
  put FuncDefs $ (MkFuncDef dclname params retTy' newexpr)::xs
  pure ()
  where
    go : (env:List (Ty, Name))
      -> {0 vars:List Name} -> Compiler.LambdaLift.Lifted vars
      -> Core (retTy:Ty ** Lifted env retTy)

    translateExpr : {0 vars:List Name}
      -> FC
      -> (env:List (Ty, Name))
      -> Ty
      -> Compiler.LambdaLift.Lifted vars
      -> Core (retTy:Ty ** Lifted env retTy)
    translateExpr fc env retTy expr = do
      r <- go env expr
      assignTy fc (retTy ** Erased EmptyFC) r
      pure r

    go env = \case
      LLocal {idx=idx} fc p =>
        let Just (ty ** n ** p) = lookupVar idx env
               | Nothing => fatal fc "translateFunction:LLocal"
         in pure $ (ty ** LLocal fc idx ty p)
      --
      LAppName fc lazy callee args => do
        args' <- traverse (go env) args
        (retTy, paramTy) <- lookupFuncSignature fc callee
        [] <- assignArgsTy fc paramTy args'
           | _ => fatal fc "translateFunction:LAppName : arity miss match"
        pure $ (retTy ** LAppName fc lazy callee retTy args')
      --
      LUnderApp fc callee missing args => do
        args' <- traverse (go env) args
        (retTy, paramTy) <- lookupFuncSignature fc callee
        rest <- assignArgsTy fc paramTy args'
        pure $ (TyObj (ObjClosure retTy (map fst rest)) ** LAppName fc Nothing callee retTy args')
      --
      LApp fc lazy clo origarg => do
        (cloty ** clo') <- go env clo 
        arg <- go env origarg
        let TyObj (ObjClosure retty paramsty@(paramty::rest)) = cloty
              | _ => fatal fc "translateFunction:LApp"
        assignTy fc (paramty ** LErased EmptyFC) arg
        case clo' of
          LApp _ _ retty' paramsty' clo'' arg'' =>
              pure $ (TyObj (ObjClosure retty' rest) ** LApp fc lazy retty' paramsty' clo'' (arg::arg''))
          _ => pure $ (TyObj (ObjClosure retty rest) ** LApp fc lazy retty (paramty::rest) clo' [arg])
      --
      LLet fc nm expr bdy => do
        (varTy ** expr') <- go env expr
        (bodyTy ** bdy') <- go ((varTy, nm)::env) bdy
        pure $ (bodyTy ** LLet fc nm varTy expr' bdy')

      LCon fc nm info tag origargs => do
        Just constr <- get ConstrDcls >>= pure . find (\x => x.dclname == nm)
           | Nothing => fatal fc "translateFunction:LCon : \{show nm} not found."
        unless (length origargs /= constr.arity) $ fatal fc "translateFunction:Lcon : arity miss match."
        args <- traverse (go env) origargs
        assignArgsTy fc 
        pure $ (TyCObj ObjData ** LCon fc nm tag args)
      --
      LOp fc lazy origop origargs => do
        args <- traverse (go env) origargs
        ty <- maybe pure freshTyVar $ opRetTy origop
        pure $ LOp fc lazy origop ty args
      --
      LExtPrim fc lazy nm origargs => do
        case nm of
          Basic "prim__codegen" => pure $ (TyCObj (ObjUInt BW8) ** LPrimVal (Str "vm0"))
          _ => do
            args <- for origargs (go env)
            retty <- freshTyVar
            pure $ (retty ** LExtPrim fc lazy nm retty args)
      --
      LConCase fc origexpr origalts origdef => do
        expr <- go env origexpr
        retty <- freshTyVar
        let retty' = (retty ** LErased emptyFC)
        alts <- for origalts $ \case
            MkLConAlt nm info tag origpargs origbody => do
              dcl <- lookupConstrDcl fc nm
              let f : List Ty -> List Name -> Core (List (Ty, Name))
                  f [] [] = []
                  f (x:xs) (y:ys) = (x, xs) :: f xs ys
                  f _ _   = fatal fc "translateFunction:LConCase:0"
              args <- f dcl.args origargs
              r@(ty ** body) <- go (parans ++ env) origbody
              assignTy retty' r
              pure $ MkConAlt nm info tag params ty body
        def <- for origdef $ \x => do
          r <- go env x
          assignTy retty' r
          pure r
        pure $ LConCase fc retty alts def

      LConstCase fc origexpr origalts origdef => do
        expr <- go env origexpr
        retty <- freshTyVar
        let retty' = (retty ** LErased emptyFC)
        alts <- for origalts $ \case
          MkConstAlt c bdy => do
            r <- go env bdy
            assignTy retty' r
            pure (c, r)
        def <- for origdef $ \x => do
          r <- go env x
          assignTy retty'  rather
          pure rather
        pure $ LConstCase fc retty expr alts def
    --
    LPrimVal fc c => pure $ (constTy c ** LPrimVal fc c)
    LErased fc    => pure $ (TyVoid ** LErased fc)
    LCrash fc msg => pure $ (TyVoid ** LCrash fc msg)







codegen : {auto c:Ref Ctxt Defs}
       -> {default [] additionalFFILangs : List String}
       -> ClosedTerm
       -> Core ()
codegen tm = do
  tyvars <- newRef TyEnv (0, [])
  condcls <- newRef ConstrDcls $ the (List ConstrDcl) []
  funcdefs <- newRef FuncDefs $ the (List FuncDef) []
  errconds <- newRef ErrConds $ the (List (Name, Lifted [] TyVoid)) []
  ffidcls <- newRef FFIDcls $ the (List FFIDcl) []

  cdata <- getCompileData False Lifted tm

  -- Phase1 : Collect signatures and give type variables of parameter.
  for_ cdata.lambdaLifted $ \case
    (nm, MkLFun args scope _) => do
      args' <- traverse (\nm' => freshTyVar >>= \ty => pure (ty, nm')) $ args ++ scope
      retty <- freshTyVar
      orig <- get FuncDefs
      put FuncDefs $ (MkFuncDef nm args' retty True (LErased EmptyFC))::orig

    (nm, MkLCon tag arity nt) => do
      args' <- for [1..arity] (\_ => freshTyVar)
      orig <- get ConstrDcls
      put ConstrDcls $ (MkConstrDcl nm arity False tag args')::orig

    (nm, MkLForeign ccs args ret) => do
      orig <- get FFIDcls
      put FFIDcls $ (MkFFIDcl nm ccs args ret)::orig

    (nm, MkLError expl) => do
      orig <- get ErrConds
      put ErrConds $ (nm, LErased EmptyFC)::orig

  -- - Argument types of constructors erased from Defs. So I need 
  --   resolve them from function body later.


  -- flip traverse_ cdata.lambdaLifted (\case
  --   (nm, MkLCon tag arity nt) => addPartialConstr nm tag arity nt
  --   (nm, MkLForeign ccs args retty) => addFFI nm ccs args retty
  --   _ => pure ())
      

--  flip traverse_ cdata.lambdaLifted (\case
--    (nm, MkLFun args scope body) => translateLambda nm body
--    _ => pure () )

  pure ()


-- --------------------------------------------------------------------------




