package one.xingyi.cddlegacy

case class LegacyData[P, R](id: String, situation: P, expected: R)
case class LegacyResult(id: String, failure: Option[String])

object LegacyResult {
  def apply[P, R](legacyData: LegacyData[P, R])(engine: P => R)(implicit failureCategoriser: FailureCategoriser[P, R]): LegacyResult =
    LegacyResult(legacyData.id, failureCategoriser(legacyData.situation, engine(legacyData.situation)))
}

trait FailureCategoriser[P, R] extends ((P, R) => Option[String])


class Legacy {

}
