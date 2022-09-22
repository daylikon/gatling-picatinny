package ru.tinkoff.gatling.profile

import cats.data._
import cats.implicits._
import java.util.regex.Pattern

sealed trait InputDataValidator {

  type ValidationResult[A] = ValidatedNec[DomainValidation, A]

  def validateAlways[T](data: T): ValidationResult[T] = {
    data.validNec
  }

  def validateHeaders(headers: Option[List[String]]): ValidationResult[Option[List[String]]] = {
    headers match {
      case Some(headersList) =>
        val regexHeader = """(.+): (.+)"""
        if (headersList.exists(header => !header.matches(regexHeader))) RequestHeadersCheck.invalidNec else headers.validNec
      case None              => headers.validNec
    }
  }

  def validateMethod(method: Option[String]): ValidationResult[Option[String]] = {
    val pattern = """^[a-zA-Z]*$"""
    val p       = Pattern.compile(pattern)
    method match {
      case Some(m) => if (p.matcher(m).find() && m.nonEmpty) method.validNec else RequestMethodCheck.invalidNec
      case None    => UndefinedMethodCheck.invalidNec
    }
  }

  def validateURI(uri: Option[String]): ValidationResult[Option[String]] = {
    val pattern = """^(?:[a-zA-Z]:)?[\/\\]{0,2}(?:[.\/\\ ](?![.\/\\\n])|[^<>:"|?*.\/\\ \n])+$"""
    val p       = Pattern.compile(pattern)
    uri match {
      case Some(u) => if (p.matcher(u).find() && uri.nonEmpty) uri.validNec else RequestURICheck.invalidNec
      case None    => UndefinedURICheck.invalidNec
    }
  }

  def validateParams(params: Option[Params]): ValidationResult[Option[Params]] = {
    params match {
      case Some(p) =>
        if (
          validateMethod(p.method).isValid &&
          validateURI(p.path).isValid &&
          validateHeaders(p.headers).isValid &&
          validateAlways(p.body).isValid
        ) params.validNec
        else IncorrectParamsCheck.invalidNec
      case None    => UndefinedParamsCheck.invalidNec
    }
  }

  def validateRequest(request: Request): ValidationResult[Request] = {
    (
      validateAlways(request.request),
      validateAlways(request.intensity),
      validateAlways(request.groups),
      validateParams(request.params),
    ).mapN(Request)
  }

  def validateRequestList(params: List[Request]): ValidationResult[List[Request]] = {
    if (params.map(req => validateRequest(req)).exists(v => v.isInvalid)) RequestListCheck.invalidNec else params.validNec
  }

}

object InputDataValidator extends InputDataValidator
