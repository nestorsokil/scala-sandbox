package crudmock.client

import crudmock.dao.{IssueDao, UserDao}
import crudmock.model._

object Client {
  def initRepo() = {
    Array(User("John Doe", "johndoe@email.com", "abcd"), User("Jane Doe", "janedoe@email.com", "efgh"))
      .foreach(UserDao.create)
  }

  def main(args: Array[String]): Unit = {
    val i = IssueDao.findById("")

    initRepo()
  }

}
