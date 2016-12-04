package crudmock.dao

import java.time.LocalDateTime

import crudmock.model._

import scala.collection.mutable

abstract class GenericDao[T <: Entity] {
  protected var repo: mutable.Set[T] = mutable.Set()

  def findById(id: String): Option[T] = repo find(_.id == id)

  def findAll(): List[T] = repo.toList

  def countAll(): Long = repo.size

  def create(entity: T) = repo add entity

  def remove(entity: T) = repo remove entity
}

object UserDao extends GenericDao[User] {
  this.repo = DataHolder.users

  def authenticate(username: String, password: String): Boolean =
    repo find(_.name == username) match {
      case None => false
      case Some(user) => user.password == password
    }
}

object ProjectDao extends GenericDao[Project] {
  this.repo = DataHolder.projects

  def findByUser(user: User): List[Project] =
    (repo filter(_.owner == user)).toList

  def findOlder(date: LocalDateTime): List[Project] =
    repo.filter(p => p.date.compareTo(date) < 0).toList

  def findNewer(date: LocalDateTime): List[Project] =
    repo.filter(p => p.date.compareTo(date) >= 0).toList

  def findIssues(project: Project): List[Issue] =
    DataHolder.issues.filter(_.project == project).toList

  def findTasks(project: Project): List[Task] =
    findIssues(project).flatMap(i => DataHolder.tasks.filter(_.issue == i))
}

object IssueDao extends GenericDao[Issue] {
  this.repo = DataHolder.issues

  def findTasks(issue: Issue): List[Task] = {
    DataHolder.tasks.filter(_.issue == issue).toList
  }
}

object TaskDao extends GenericDao[Task] {
  this.repo = DataHolder.tasks
}