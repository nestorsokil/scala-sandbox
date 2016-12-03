package crudmock.dao

import crudmock.model._

import scala.collection.mutable

object DataHolder {
  val users = mutable.Set[User]()
  val projects = mutable.Set[Project]()
  val issues = mutable.Set[Issue]()
  val tasks = mutable.Set[Task]()
}
