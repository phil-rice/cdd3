package one.xingyi.cddexamples.qAndA
import one.xingyi.cddutilities.functions.Lens
import one.xingyi.cddutilities.json.{JsonList, JsonObject, JsonWriter}

case class Fact[DS, T](id: String, lens: Lens[DS, T])

case class IndustrySector(s: String)
case class Address(s: String)
case class Website(s: String)
case class TeleNo(s: String)
case class BIC(s: String)
case class ClearingCode(s: String)
case class Chaps(s: String)

case class Naics(code: String)
case class Nace(code: Int)
case class Identity(name: String,
                    industrySectory: IndustrySector,
                    registeredAddress: Address,
                    operationAddress: Address,
                    teleNo: TeleNo, fax: TeleNo,
                    website: Website,
                    bIC: BIC,
                    chaps: Chaps,
                    clearingCode: ClearingCode)

case class GBP(amnt: Int)
case class BalanceSheet(totalNetAssets: GBP, totalLiabilities: GBP, shareHoldersInterest: GBP)
case class ProfitAndLoss(nettIncome: GBP, nettExpenditure: GBP)
case class FinancialData(balanceSheet: BalanceSheet, profitAndLoss: ProfitAndLoss)

case class Activities(mainActivities: String, productAndServices: String, businessLine: String, naics: Naics, nace: Nace)

case class GateKeeperThings(bearerShares: Boolean)

case class PartialData[T](t: T, fn: T => List[String]){
  def isComplete= fn(t).isEmpty
  def isNotComplete= !isComplete
  }
case class Entity(identity: PartialData[Identity], financialData: PartialData[FinancialData], activities: PartialData[Activities], gateKeeperThings: PartialData[GateKeeperThings])

trait Manipulate[DS] {
  val identity: Lens[DS, Identity] = ???
  val status: Lens[DS, Boolean] = ???
  val balanceSheet: Lens[DS, BalanceSheet] = ???
  val profitAndLoss: Lens[DS, ProfitAndLoss] = ???
  val activites: Lens[DS, Activities] = ???
}

trait Question[T] extends (T => JsonObject)


object Decision{

  def nextDataToFind(e: Entity) = {
    if (e.identity.isNotComplete) "displayIdentityQuestions" else
      if (e.financialData.isNotComplete) "displayFinancials" else
      if (e.gateKeeperThings.isNotComplete) "DisplayGateKeeperThings" else
         nowCalculateResult
  }

  def nowCalculateResult = ???
}


case class NewQuestion(idToDisplayTheQuestion: String)
case class ProcessResponse[DS](idForAValue: String, lens: Lens[DS, String])
import one.xingyi.cddutilities.json.JsonLanguage._
object Question {




  def nameQuestion: Question[String] = name => JsonObject("questionText" -> "Name", "formType" -> "text", "value" -> name)
  def addressQuestion(addressName: String): Question[String] = address => JsonObject("questionText" -> address, "formType" -> "text", "value" -> address)
  def teleNoQuestion: Question[String] = address => JsonObject("questionText" -> "Telephone No", "formType" -> "text", "value" -> address)
  def faxNoQuestion: Question[String] = address => JsonObject("questionText" -> "Fax No", "formType" -> "text", "value" -> address)
  def bicsQuestion: Question[String] = address => JsonObject("questionText" -> "Fax No", "formType" -> "text", "value" -> address)
  def chapsQuestion: Question[String] = address => JsonObject("questionText" -> "Fax No", "formType" -> "text", "value" -> address)
  def clearingCodeQuestion: Question[String] = address => JsonObject("questionText" -> "Fax No", "formType" -> "text", "value" -> address)

  def identityQuestion: Question[Identity] = identity => section(
    nameQuestion,
    addressQuestion,
    teleNoQuestion,
    faxNoQuestion,
    bicsQuestion,
    chapsQuestion,
    clearingCodeQuestion
  )

  def totalNetAssetsQ: Question[GBP] = assets => JsonObject("questionText" -> "What are the total net assets", "formType" -> "text", "value" -> assets)
  def totalLiabilities: Question[GBP] = totalLiabilities => JsonObject("questionText" -> "What are the totalLiabilities", "formType" -> "text", "value" -> totalLiabilities)
  def shareHoldersQ: Question[GBP] = shareHoldersInterest => JsonObject("questionText" -> "What are the totalLiabilities", "formType" -> "text", "value" -> totalLiabilities)
  def balanceSheetQ: Question[BalanceSheet] = section(totalNetAssetsQ, totalLiabilities, shareHoldersQ)


  def netIncomeQ: Question[GBP] = nett => JsonObject("questionText" -> "What is the net income", "formType" -> "text", "value" -> nett.amnt)
  def netExpenditure: Question[GBP] = nett => JsonObject("questionText" -> "What is the net expenditure", "formType" -> "text", "value" -> nett.amnt)
  def profileAndLossQuestiob: Question[ProfitAndLoss] = section(netIncomeQ, netExpenditure)
  def financeQuestion: Question[FinancialData] = section(balanceSheetQ, profileAndLossQuestiob)
  def gateKeeperQuestion: Question[GateKeeperThings] = ??? //section(balanceSheetQ, profileAndLossQuestiob)


  //  def identityQuestion[J]: Question[J, Identity] = JsonObject("children" -> JsonList(List(


}
