package tastyquery.util

import compiletime.constValue
import compiletime.error
import compiletime.summonAll

sealed abstract class FlagMirror[A <: reflect.Enum]():
  outer =>

  opaque type FlagSet = Long

  def EmptyFlagSet: FlagSet = 0L

  object exports:
    type FlagSet = outer.FlagSet
    export outer.EmptyFlagSet

  // avoid comparisons between `A` and `FlagSet`
  given CanEqual[FlagSet, FlagSet] = CanEqual.derived
  given CanEqual[A, A] = CanEqual.derived

  // methods on `FlagSet`
  object FlagSet:
    extension (flags: FlagSet)
      def isEmpty: Boolean = flags == EmptyFlagSet
      def |(flag: A): FlagSet = flags | flag.toFlags
      def is(flag: A): Boolean = (flags & flag.toFlags) != EmptyFlagSet

  // methods on `A`
  extension (flag: A) private def toFlags: FlagSet = 1L << flag.ordinal

object FlagMirror:

  transparent inline def exports[A <: reflect.Enum](using m: FlagMirror[A]): m.exports.type = m.exports

  inline def derived[A <: reflect.Enum](using m: deriving.Mirror.SumOf[A]): FlagMirror[A] =
    inline if constValue[Tuple.Size[m.MirroredElemTypes]] <= 63 then
      summonAll[Tuple.Map[m.MirroredElemTypes, [S] =>> S <:< Singleton]] // assert all singleton
      new FlagMirror[A]() {}
    else error("FlagsMirror only supports up to 63 flags")
