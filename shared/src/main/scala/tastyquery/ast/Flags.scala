package tastyquery.ast

import tastyquery.util.FlagMirror

enum Flags derives FlagMirror:
  case Method

object Flags:
  export FlagMirror.exports.{*, given}
