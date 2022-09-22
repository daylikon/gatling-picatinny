package ru.tinkoff.gatling.profile

sealed trait DomainValidation {
  def errorMessage: String
}

case object RequestListCheck extends DomainValidation {
  def errorMessage: String =
    "\nError with requests in profile."
}

case object RequestHeadersCheck extends DomainValidation {
  def errorMessage: String =
    "\nCheck headers in profile. Format is 'header: data'."
}

case object RequestMethodCheck extends DomainValidation {
  def errorMessage: String =
    "\nIncorrect method string in profile."
}

case object RequestURICheck extends DomainValidation {
  def errorMessage: String =
    "\nIncorrect URI in profile."
}

case object IncorrectParamsCheck extends DomainValidation {
  def errorMessage: String =
    "\nIncorrect params for request in profile."
}

case object UndefinedMethodCheck extends DomainValidation {
  def errorMessage: String =
    "\nUndefined method for request in profile."
}

case object UndefinedURICheck extends DomainValidation {
  def errorMessage: String =
    "\nUndefined uri for request in profile."
}

case object UndefinedParamsCheck extends DomainValidation {
  def errorMessage: String =
    "\nUndefined params for request in profile."
}
