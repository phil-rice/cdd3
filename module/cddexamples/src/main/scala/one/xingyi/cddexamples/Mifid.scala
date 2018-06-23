package one.xingyi.cddexamples
import one.xingyi.cddengine.{Engine, SimpleValidation, UseCase1}
import one.xingyi.cddexamples
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
case class Money(i: Long) extends AnyVal {
  def >=(other: Money): Boolean = i >= other.i
}

sealed trait MifidConclusion
case object ProfessionalClient extends MifidConclusion
case object RetailClient extends MifidConclusion


case class Config(balanceSheetThreshold: Money, netTurnoverThreshold: Money, ownFundsThreshold: Money)

case class Entity(name: String, businessType: BusinessType, balanceSheet: Money, netTurnover: Money, ownFunds: Money, mainBusinessIsFinancialTransactions: Boolean = false)

case class EntityDetails(entity: Entity)(implicit config: Config) {
  def bigBalanceSheet = entity.balanceSheet >= config.balanceSheetThreshold
  def highTurnoverSheet = entity.netTurnover >= config.netTurnoverThreshold
  def highOwnFunds = entity.ownFunds >= config.ownFundsThreshold
  val countOfHighMoneyMakers = List(bigBalanceSheet, highTurnoverSheet, highOwnFunds).count(_ == true)
  override def toString: String = s"EntityDetais($entity, ($bigBalanceSheet, $highTurnoverSheet, $highOwnFunds), count $countOfHighMoneyMakers)"
}


class MifidDecisionMaker {
  type MifidUC = UseCase1[EntityDetails, MifidConclusion]
import one.xingyi.cddutilities.language.AnyLanguage._
  implicit class BusinessTypeOps(b: BusinessType)(implicit config: Config) {
    case class name(name: String) {
      def lotsOfMoney = EntityDetails(Entity(name, b, balanceSheet = Money(1000000000), netTurnover = Money(1000000000), ownFunds = Money(1000000000)))
      def highBalanceSheet = EntityDetails(Entity(name, b, balanceSheet = Money(1000000000), netTurnover = Money(0), ownFunds = Money(0))) sideeffect(s => println(s"High balance sheet $s"))
      def highNetTurnover = EntityDetails(Entity(name, b, balanceSheet = Money(0), netTurnover = Money(1000000000), ownFunds = Money(0)))
      def highOwnFunds = EntityDetails(Entity(name, b, balanceSheet = Money(0), netTurnover = Money(0), ownFunds = Money(100000)))
      def highBSAndOwnFunds = EntityDetails(Entity(name, b, balanceSheet = Money(1000000000), netTurnover = Money(0), ownFunds = Money(1000000000)))
      def highTurnoverAndBS = EntityDetails(Entity(name, b, balanceSheet = Money(0), netTurnover = Money(1000000000), ownFunds = Money(1000000000)))
      def totallyBrokeAndInviolationofGAP7fold = EntityDetails(Entity(name, b, balanceSheet = Money(0), netTurnover = Money(0), ownFunds = Money(0)))
      def mainBuisnessIsInvestment = EntityDetails(Entity(name, b, Money(0), Money(0), Money(0), mainBusinessIsFinancialTransactions = true))
    }
  }
  import BusinessType._

  implicit val config = Config(balanceSheetThreshold = Money(20000000), netTurnoverThreshold = Money(400000000), ownFundsThreshold = Money(2000000))

  val ucAuthorisedOrRegulatedEntites = new MifidUC("Authorised or Regulated entities") {
    scenario(creditInstitute name "Credit Suisse" lotsOfMoney) produces ProfessionalClient when (ed => authorisedForFinancialMarkets.contains(ed.entity.businessType))
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
    scenario(nationalGovernment name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient when (ed => nationalRegionalBodiesOrSimilar.contains(ed.entity.businessType))
    scenario(regionalGovernment name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient
    scenario(publicBodiesThatManagePublicDebt name "UK PLC" totallyBrokeAndInviolationofGAP7fold) produces ProfessionalClient
  }

  val ucInstituionalInvestorsWhoseMainActivityIsToInves = new MifidUC("we invest a lot") {
    scenario(other name "invest a lot" mainBuisnessIsInvestment) produces cddexamples.ProfessionalClient when (ed => ed.entity.mainBusinessIsFinancialTransactions)
  }
  val someOtherComplicatedThing = new MifidUC("some other complicated rules") {
  }

  val categoriser = Engine(ucLargeUndertaking or ucAuthorisedOrRegulatedEntites  or ucNationalAndRegionalBodies or ucInstituionalInvestorsWhoseMainActivityIsToInves)
  val categoriser2 = Engine(ucAuthorisedOrRegulatedEntites or ucLargeUndertaking or ucNationalAndRegionalBodies)
  val categoriser3 = Engine(ucAuthorisedOrRegulatedEntites or ucLargeUndertaking or ucNationalAndRegionalBodies or someOtherComplicatedThing)


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

