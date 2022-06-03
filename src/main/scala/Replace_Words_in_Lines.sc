def replaceWords(
                  lines: Seq[String],
                  replacements: Map[String, String]): Seq[String] = {

  lines
    .foldLeft(Seq.empty[String]) { (result, line) => result ++ line.split("\\s+")
    .map(w => replacements.getOrElse(w.toLowerCase, w))}

}

replaceWords(
  lines = Seq(
    "Good morning. Nice to see you",
    "Dzien dobry. Jak idzie?"),
  replacements = Map(
    "good" -> "Dobry",
    "see" -> "XXX",
    "jak" -> "How"
  )
)


