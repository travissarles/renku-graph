package io.renku.jena

object GitLab {

  def validate(token: Array[Char]): Option[Array[Char]] = Some(token)

  private val usersProjects: Map[String, Set[String]] = Map(
    "kuba" -> Set("https://dev.renku.ch/projects/rokroskar/datasets-test")
  )

  // users roles can be read from the projects endpoint (look here: https://docs.gitlab.com/ee/api/projects.html)
  // from the path_with_namespace field
  // remember about the `simple=true` query param!
  def findProjects(userName: String): Set[String] = usersProjects.getOrElse(userName, Set.empty)
}
