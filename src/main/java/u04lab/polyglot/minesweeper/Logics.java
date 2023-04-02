package u04lab.polyglot.minesweeper;

public interface Logics {

    void toggleFlag(Pair<Integer, Integer> cellPosition);

    boolean hasMine(Pair<Integer, Integer> cellPosition);

    boolean hasFlag(Pair<Integer, Integer> cellPosition);

    boolean isRevealed(Pair<Integer, Integer> cellPosition);

    int getNeighbourMines(Pair<Integer, Integer> cellPosition);

    boolean hit(Pair<Integer, Integer> cellPosition);

    boolean won();
}
