package io.renku.jena.realm

import org.apache.shiro.config.Ini
import org.apache.shiro.realm.text.IniRealm
import scala.collection.JavaConverters._
import scala.util.Try

object PrivilegedUsers {

  private val configFileName = "run/shiro.ini"

  def get(username: String): Option[User] = allUsers.find(_.username == username)

  val Admin: User = loadCredentials("admin")
  val Renku: User = loadCredentials("renku")

  private val allUsers: Set[User] = Set(Admin, Renku)

  def contains(username: String): Boolean = allUsers.exists(_.username == username)

  private lazy val users: Map[String, String] = Try {
    val ini = Ini.fromResourcePath(configFileName)
    Option(ini getSection IniRealm.USERS_SECTION_NAME).map(_.asScala.toMap).getOrElse(Map.empty)
  }.fold(e => throw new Exception(s"$configFileName cannot be found", e), identity)

  private def loadCredentials(username: String): User =
    users
      .get(username)
      .map(User(username, _))
      .getOrElse(throw new Exception(s"No credentials found for '$username' in the $configFileName"))

  lazy val verifyConfigLoaded: Unit = allUsers.nonEmpty
}

final case class User(username: String, password: String)
