package u04lab.polyglot.minesweeper;

public class Position extends Pair<Integer, Integer> {

    public Position(final int row, final int column) {
        super(row, column);
    }

    public Position(final Pair<Integer, Integer> position) {
        super(position.getX(), position.getY());
    }

    public int row() {
        return getX();
    }

    public int column() {
        return getY();
    }
}
