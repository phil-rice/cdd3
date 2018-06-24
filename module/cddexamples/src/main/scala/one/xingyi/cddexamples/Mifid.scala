package one.xingyi.cddexamples
import one.xingyi.cddengine.{Engine, SimpleValidation, UseCase1}
import one.xingyi.cddexamples
import one.xingyi.cddexamples.qAndA._
import one.xingyi.cddmustache.Mustache

import scala.language.postfixOps
import scala.language.implicitConversions

object BusinessType {
  val creditInstitute = BusinessType("credit institute")
  val investmentFirm = BusinessType("investmentFirm")
  val financialInstitution = BusinessType("financialInstitution")
  val insuranceCompany = BusinessType("insuranceCompany")
  val collectiveInvestmentScheme = BusinessType("collectiveInvestmentScheme")
  val pensionFund = BusinessType("pensionFund")
  val comodityDealer = BusinessType("comodityDealer")
  val locals = BusinessType("locals")
  val someInstitutionalDealer = BusinessType("someInstitutionalDealer")
  val authorisedForFinancialMarkets = List(creditInstitute, investmentFirm, financialInstitution, collectiveInvestmentScheme, pensionFund, comodityDealer, someInstitutionalDealer)
  //
  val nationalGovernment = BusinessType("national government")
  val regionalGovernment = BusinessType("regional government")
  val publicBodiesThatManagePublicDebt = BusinessType("publicBodiesThatManagePublicDebt")
  val someInstitutionalDealerInvestingInFinancialInstrument = BusinessType("someInstitutionalDealerInvestingInFinancialInstrument")
  val nationalRegionalBodiesOrSimilar = List(nationalGovernment, regionalGovernment, publicBodiesThatManagePublicDebt)

  val other = BusinessType("other")
}
case class BusinessType(s: String) extends AnyVal


sealed trait MifidConclusion
case object UnknownClient extends MifidConclusion
case object ProfessionalClient extends MifidConclusion
case object RetailClient extends MifidConclusion


case class Config(balanceSheetThreshold: GBP, netTurnoverThreshold: GBP, ownFundsThreshold: GBP)


case class EntityDetails(entity: Entity)(implicit config: Config, blackboard: Blackboard[Entity]) {
  def bigBalanceSheet = entity.financialData.balanceSheet.total >= config.balanceSheetThreshold
  def highTurnoverSheet = entity.financialData.profitAndLoss.turnover >= config.netTurnoverThreshold
  def highOwnFunds = entity.financialData.balanceSheet.shareHoldersInterest >= config.ownFundsThreshold
  val countOfHighMoneyMakers = List(bigBalanceSheet, highTurnoverSheet, highOwnFunds).count(_ == true)
  def hasValidationIssues = {val validation= blackboard.children.flatMap(_.validate(entity));; println(validation + " " + validation.isEmpty); validation.nonEmpty}
  override def toString: String = s"EntityDetais($entity, ($bigBalanceSheet, $highTurnoverSheet, $highOwnFunds), count $countOfHighMoneyMakers)"
}


class MifidDecisionMaker extends Question{
  type MifidUC = UseCase1[EntityDetails, MifidConclusion]
  import one.xingyi.cddutilities.language.AnyLanguage._


  private val prototypeFinancialData = FinancialData(BalanceSheet(totalNetAssets = GBP(123), totalLiabilities = GBP(234), shareHoldersInterest = GBP(0)), ProfitAndLoss(nettIncome = GBP(12323), nettExpenditure = GBP(234)))
  private val prototypeActivities = Activities("someMainActivies", false, "someProductAndServices", "someBuisnessLine", Naics("someNaicsCode"), Nace(144))
  val prototypeEntity = Entity(
    Identity("", BusinessType(""), IndustrySector("something"), Address(""), Address(""), TeleNo("phoneNo"), TeleNo("fax"), Website(""), BIC("someBic"), Chaps("someChaps"), ClearingCode("someClearingCode")),
    prototypeFinancialData,
    prototypeActivities,
    GateKeeperThings(false)
  )
  def edWith(name: String, businessType: BusinessType, balanceSheetTotal: Long, netTurnover: Long, ownFunds: Long, mainBusinessIsFinancialTransactions: Boolean = false)(implicit config: Config, blackboard: Blackboard[Entity]) = {
    val fd = prototypeFinancialData.
             copy(balanceSheet = prototypeFinancialData.balanceSheet.copy(totalNetAssets = GBP(balanceSheetTotal), shareHoldersInterest = GBP(ownFunds))).
             copy(profitAndLoss = prototypeFinancialData.profitAndLoss.copy(nettIncome = GBP(netTurnover)))

    val identity = prototypeEntity.identity.copy(name = name, businessType = businessType)
    val activities = prototypeActivities.copy(mainBusinessIsFinancialTransactions = mainBusinessIsFinancialTransactions)
    EntityDetails(prototypeEntity.copy(identity = identity, financialData = fd, activities = activities))
  }
  implicit class BusinessTypeOps(b: BusinessType)(implicit config: Config, blackboard: Blackboard[Entity]) {
    case class name(name: String) {
      def lotsOfMoney = edWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 1000000000, ownFunds = 1000000000)
      def highBalanceSheet = edWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 0, ownFunds = 0)
      def highNetTurnover = edWith(name, b, balanceSheetTotal = 0, netTurnover = 1000000000, ownFunds = 0)
      def highOwnFunds = edWith(name, b, balanceSheetTotal = 0, netTurnover = 0, ownFunds = 1000000000)
      def highBSAndOwnFunds = edWith(name, b, balanceSheetTotal = 1000000000, netTurnover = 0, ownFunds = 1000000000)
      def highTurnoverAndBS = edWith(name, b, balanceSheetTotal = 0, netTurnover = 1000000000, ownFunds = 1000000000)
      def totallyBrokeAndInviolationofGAP7fold = edWith(name, b, balanceSheetTotal = 0, netTurnover = 0, ownFunds = 0)
      def mainBuisnessIsInvestment = edWith(name, b, 0, 0, 0, mainBusinessIsFinancialTransactions = true)
    }
  }
  import BusinessType._

  implicit val config = Config(balanceSheetThreshold = GBP(20000000), netTurnoverThreshold = GBP(400000000), ownFundsThreshold = GBP(2000000))

  val ucMustBeValidated = new MifidUC("No validation issues"){
    scenario(investmentFirm name "" lotsOfMoney) produces UnknownClient when {ed => println(s"in when: ${ed.hasValidationIssues} $ed"); ed.hasValidationIssues}

  }
  val ucAuthorisedOrRegulatedEntites = new MifidUC("Authorised or Regulated entities") {
    scenario(creditInstitute name "Credit Suisse" lotsOfMoney) produces ProfessionalClient when (ed => authorisedForFinancialMarkets.contains(ed.entity.identity.businessType))
    scenario(investmentFirm name "UBS" lotsOfMoney) produces ProfessionalClient
    scenario(financialInstitution name "HSBC" lotsOfMoney) produces ProfessionalClient
    scenario(insuranceCompany name "Scottish Widow" lotsOfMoney) produces ProfessionalClient
    scenario(collectiveInvestmentScheme name "Hedgehog" lotsOfMoney) produces ProfessionalClient
    scenario(pensionFund name "Lacuna Inc" lotsOfMoney) produces ProfessionalClient
    scenario(comodityDealer name "gamblers R Us" lotsOfMoney) produces ProfessionalClient
    scenario(locals name "Pete's Sweeps" lotsOfMoney) produces ProfessionalClient
    scenario(someInstitutionalDealer name "vegas inc" lotsOfMoney) produces ProfessionalClient
  }

  val ucLargeUndertaking = new MifidUC("Has two of ' high balance sheet, hit turnover, high own funds' ") {
    scenario(other name "really rich guy inc" lotsOfMoney) produces ProfessionalClient when (ed => ed.countOfHighMoneyMakers >= 2)
    scenario(other name "HighBs High own funds" highBSAndOwnFunds) produces ProfessionalClient

    scenario(other name "High BS, nothing else" highBalanceSheet) produces RetailClient when (ed => ed.countOfHighMoneyMakers < 2)
    scenario(other name "High turnover, nothing else" highNetTurnover) produces RetailClient
    scenario(other name "High ownfunds, nothing else" highOwnFunds) produces RetailClient
  }

  val ucNationalAndRegionalBodies = new MifidUC("National and regional body") {
    scenario(nationalGovernment name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient when (ed => nationalRegionalBodiesOrSimilar.contains(ed.entity.identity.businessType))
    scenario(regionalGovernment name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient
    scenario(publicBodiesThatManagePublicDebt name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient
  }

  val ucInstituionalInvestorsWhoseMainActivityIsToInves = new MifidUC("we invest a lot") {
    scenario(other name "invest a lot" mainBuisnessIsInvestment) produces cddexamples.ProfessionalClient when (ed => ed.entity.activities.mainBusinessIsFinancialTransactions)
  }
  val someOtherComplicatedThing = new MifidUC("some other complicated rules") {
  }

  val categoriser = Engine(ucMustBeValidated or ucLargeUndertaking or ucAuthorisedOrRegulatedEntites or ucNationalAndRegionalBodies or ucInstituionalInvestorsWhoseMainActivityIsToInves)
//  println(ucMustBeValidated.allScenarios)
//  val categoriser2 = Engine(ucAuthorisedOrRegulatedEntites or ucLargeUndertaking or ucNationalAndRegionalBodies)
//  val categoriser3 = Engine(ucAuthorisedOrRegulatedEntites or ucLargeUndertaking or ucNationalAndRegionalBodies or someOtherComplicatedThing)


}


object Trace extends App {
  import Mustache._
  import one.xingyi.json4s.Json4s._

  implicit def v[P, R] = new SimpleValidation[P, R]

  private val categoriser: Engine[EntityDetails, MifidConclusion] = new MifidDecisionMaker().categoriser
  categoriser.tools.trace("mifid")
  println("Issues")
  categoriser.tools.issues.foreach(println)
}

