package tastyquery.ast

import tastyquery.ast.Names.TypeName

enum ParamSig:
  case ParamName(typ: TypeName)
  case TyParamLen(len: Int)

case class Signature(paramsSig: List[ParamSig], resSig: TypeName)
