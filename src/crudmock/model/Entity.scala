package crudmock.model

import java.time.LocalDateTime

abstract class Entity(val id: String)

case class User(override val id: String, name: String, email: String, password: String)
  extends Entity(id)

object User{
  var uid = 0
  def apply(name: String, email: String, password: String): User = {
    uid += 1
    new User(uid.toString, name, email, password)
  }
}

case class Project(override val id: String, name: String, owner: User, date: LocalDateTime)
  extends Entity(id)

object Project

case class Issue(override val id: String, title: String, author: User, project: Project, date: LocalDateTime)
  extends Entity(id)

object Issue

case class Task(override val id: String, desc: String, owner: User, issue: Issue)
  extends Entity(id)

object Task