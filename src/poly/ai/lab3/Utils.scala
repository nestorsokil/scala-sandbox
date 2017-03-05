package poly.ai.lab3

/**
  * Created by Admin on 05.03.2017.
  */
object Utils {
  def concat(s1:String, s2:String, s3:String): String = {
    var res = ""
    if(!s1.isEmpty) res = s1
    if(!s2.isEmpty) {
      if (!res.isEmpty)
        res += " "
      res += s2
    }
    if(!s3.isEmpty) {
      if (!res.isEmpty)
        res += " "
      res += s3
    }
    res
  }
}
