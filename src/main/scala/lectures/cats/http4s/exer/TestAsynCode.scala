package lectures.cats.http4s.exer

object TestAsynCode extends App {
  import scala.concurrent.Future

  trait UptimeClient {
    def getUptime(hostname: String): Future[Int]
  }

  import cats.instances.future._ // for Applicative
  import cats.instances.list._ // for Traverse
  import cats.syntax.traverse._ // for traverse
  import scala.concurrent.ExecutionContext.Implicits.global

  class UptimeService(client: UptimeClient) {
    def getTotalUptime(hostnames: List[String]): Future[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient {
    def getUptime(hostname: String): Future[Int] =
      Future.successful(hosts.getOrElse(hostname, 0))
  }

  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
//  testTotalUptime()

  import cats.Id
  trait UptimeClient1[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  trait RealUptimeClient extends UptimeClient1[Future] {
    def getUptime(hostname: String): Future[Int]
  }

  trait TestUptimeClient1 extends UptimeClient1[Id] {
    def getUptime(hostname: String): Int
  }

  class TestUptimeClient2(hosts: Map[String, Int])
    extends UptimeClient1[Id] {
    def getUptime(hostname: String): Id[Int] =
      hosts.getOrElse(hostname, 0)
  }

  import cats.Applicative
  import cats.syntax.functor._ // for map
  class UptimeService1[F[_]](client: UptimeClient1[F])(implicit a: Applicative[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  def testTotalUptime1() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient2(hosts)
    val service = new UptimeService1(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime1()
}
