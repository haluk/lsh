package preprocess

/**
  * Created by hd on 3/4/17.
  */
class Preprocess() {

  def replaceAllWhiteSpace(document: String) = {
    document.replaceAll("\\s", " ")
  }
  def removePunctuationMarks(document: String) = {
    document.replaceAll("""([\p{Punct}])""", "")
  }

  def removeStopWord(document: String) = {
    document.split(" ").filterNot(x => x.size < 3 || x.endsWith(sys.props("line.separator")) || x.equals("the")).mkString(" ")
  }

  def toLowerCase(document: String) = {
    document.toLowerCase()
  }

  def process(document: String) = {
    toLowerCase(removeStopWord(removePunctuationMarks(replaceAllWhiteSpace(document))))
  }

}
