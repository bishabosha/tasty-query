package tastyquery.reader

import dotty.tools.tasty.UnpickleException

type Result[+T] = Either[UnpickleException, T]
