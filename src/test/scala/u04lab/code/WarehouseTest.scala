package u04lab.code

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.{Before, Test}
import u04lab.code.List.cons
import u04lab.code.List.empty

class WarehouseTest :

  private val nonExistingItemCode = 0
  private val dellXps: Item = Item(33, "Dell XPS 15", cons("notebook", empty))
  private val dellInspiron: Item = Item(34, "Dell Inspiron 13", cons("notebook", empty))
  private val xiaomiMoped: Item = Item(35, "Xiaomi S1", cons("moped", cons("mobility", empty)))
  private val warehouse: Warehouse = Warehouse()

  @Before def setup(): Unit =
    warehouse.store(dellXps)
    warehouse.store(dellInspiron)
    warehouse.store(xiaomiMoped)

  @Test def testContains(): Unit =
    assertTrue(warehouse.contains(dellXps.code))
    assertTrue(warehouse.contains(dellInspiron.code))
    assertTrue(warehouse.contains(xiaomiMoped.code))
    assertFalse(warehouse.contains(nonExistingItemCode))

  @Test def testStore(): Unit =
    val mac = Item(36, "Mac", cons("desktop", empty))
    warehouse.store(mac)
    assertTrue(warehouse.contains(mac.code))

  @Test def testRetrieve(): Unit =
    assertEquals(Option.None(), warehouse.retrieve(nonExistingItemCode))
    assertEquals(Option.Some(dellXps), warehouse.retrieve(dellXps.code))

  @Test def testSearch(): Unit =
    assertEquals(cons(xiaomiMoped, empty), warehouse.searchItems("mobility"))
    assertEquals(cons(dellXps, cons(dellInspiron, empty)), warehouse.searchItems("notebook"))
    assertEquals(empty, warehouse.searchItems("null"))

  @Test def testRemove(): Unit =
    warehouse.remove(dellXps)
    assertEquals(Option.None(), warehouse.retrieve(dellXps.code))