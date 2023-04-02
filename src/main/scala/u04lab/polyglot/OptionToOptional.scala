package u04lab.polyglot
import u04lab.code.Option
import u04lab.code.Option.*
import java.util.Optional

object OptionToOptional:

  def apply(option: Option[Int]): Optional[Integer] = option match
    case Some(a) => Optional.of(a)
    case _ => Optional.empty()

