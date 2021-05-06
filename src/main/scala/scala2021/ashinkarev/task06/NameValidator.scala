package scala2021.ashinkarev.task06

object NameValidator {
  var latinLetters = ('a' to 'z') ++ ('A' to 'Z');

  // Найти имя менеджера департамента, в котором работает сотрудник по имени сотрудника
  def isValidName(name: String): Either[String, Boolean] = {
    if (name.isEmpty()) {
      return Right(true);
    }

    val nonLatinSymbols = name
      .filter((symbol) => !latinLetters.contains(symbol))
      .distinct
      .toList
      .map((symbol) => if (symbol.isWhitespace) "whitespace" else symbol.toString());

    return if (nonLatinSymbols.isEmpty) Right(true) else Left(s"Only latin letters are allowed. Those are not allowed symbols: ${nonLatinSymbols.mkString(" ")}.")
  }
}