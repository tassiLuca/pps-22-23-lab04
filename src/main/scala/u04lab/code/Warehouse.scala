package u04lab.code
import List.*
trait Item:
  def code: Int
  def name: String
  def tags: List[String]


object Item:
  def apply(code: Int, name: String, tags: List[String] = List.empty): Item = ItemImpl(code, name, tags)

  private case class ItemImpl(code: Int, name: String, tags: List[String]) extends Item

/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:

  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit

  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): List[Item]

  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Option[Item]

  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit

  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean

object Warehouse:
  def apply(): Warehouse = WarehouseImpl()

  private class WarehouseImpl extends Warehouse:
    import List.*
    import Option.*
    private val items: List[Item] = Nil()

    override def store(item: Item): Unit = append(items, of(item))

    override def contains(itemCode: Int): Boolean = isPresent(retrieve(itemCode))

    override def retrieve(code: Int): Option[Item] = find(items)(i => i.code == code)

    override def searchItems(tag: String): List[Item] = filter(items)(i => List.contains(i.tags, tag))

    override def remove(item: Item): Unit = List.remove(items)(_ == item)
end Warehouse

@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()

  val dellXps = Item(33, "Dell XPS 15", cons("notebook", empty))
  val dellInspiron = Item(34, "Dell Inspiron 13", cons("notebook", empty))
  val xiaomiMoped = Item(35, "Xiaomi S1", cons("moped", cons("mobility", empty)))

  warehouse.contains(dellXps.code) // false
  warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  warehouse.contains(dellXps.code) // true
  warehouse.store(dellInspiron) // side effect, add dell inspiron to the warehouse
  warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  warehouse.searchItems("mobility") // List(xiaomiMoped)
  warehouse.searchItems("notebook") // List(dellXps, dellInspiron)
  warehouse.retrieve(11) // None
  warehouse.retrieve(dellXps.code) // Some(dellXps)
  warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  warehouse.retrieve(dellXps.code) // None

/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/