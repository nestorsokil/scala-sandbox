package crudmock.service
import java.time.LocalDateTime

import crudmock.dao.{DataHolder, IssueDao, ProjectDao}
import crudmock.model._

class TaskService {
  def complete(user: User, task: Task) = {
    if(task.owner == user){
      DataHolder.tasks.remove(task)
      DataHolder.tasks.add(task.copy(isCompleted = true))

      val issue = task.issue
      if(IssueDao.findTasks(issue).forall(_.isCompleted)) {
        val closed = issue.copy(isClosed = true, dateClosed = LocalDateTime.now())
        DataHolder.issues.remove(issue)
        DataHolder.issues.add(closed)
      }
    }
  }
}
