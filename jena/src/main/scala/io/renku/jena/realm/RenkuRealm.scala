package io.renku.jena.realm

import io.renku.jena.GitLab
import org.apache.shiro.authc._
import org.apache.shiro.authz.{AuthorizationException, AuthorizationInfo, SimpleAuthorizationInfo}
import org.apache.shiro.realm.AuthorizingRealm
import org.apache.shiro.subject.PrincipalCollection

import scala.collection.JavaConverters._

class RenkuRealm extends AuthorizingRealm {

  PrivilegedUsers.verifyConfigLoaded

  override def doGetAuthorizationInfo(principals: PrincipalCollection): AuthorizationInfo = {
    for {
      princps  <- Option(principals)
      username <- findPrincipal(princps)
      roles    <- PrivilegedUsers.get(username).map(_ => Set.empty[String]) orElse Some(GitLab findProjects username)
    } yield new SimpleAuthorizationInfo(roles.asJava)
  } getOrElse (throw new AuthorizationException("Cannot find authorization info"))

  private def findPrincipal(principal: PrincipalCollection): Option[String] =
    Option(getAvailablePrincipal(principal)) flatMap {
      case name: String => Some(name)
      case _ => None
    }

  override def doGetAuthenticationInfo(token: AuthenticationToken): AuthenticationInfo = token match {
    case token: UsernamePasswordToken => {
      println(s"RenkuRealm: returning auth info for admin; given token: ${token.getUsername}")
      for {
        username <- Option(token.getUsername)
        password <- PrivilegedUsers.get(username).map(_.password.toCharArray) orElse GitLab.validate(token.getPassword)
      } yield new SimpleAuthenticationInfo(username, password, getName)
    } getOrElse (throw new UnknownAccountException("No account found"))
    case token => throw new UnknownAccountException(s"$token is unknown")
  }
}
