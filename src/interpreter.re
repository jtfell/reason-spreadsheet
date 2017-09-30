open Cell;

/*
   The actual interpreter. It exposes a simple top-level function that
   takes a 2D array of cells and returns the same type.

   Conceptual approach:

   - Initial pass over each cell in the grid:
     * Empty cells result in empty cells
     * Normal values result in being displayed normally
     * Anything prefixed with "=" is put in a FIFO queue

   - Iterate over queue:
     * Parse expression and look for references to other cells
     * If all referenced cells are already resolved, calculate the expression and
       flag this cell as resolved
     * If at least 1 referenced cell is unresolved, put back on the queue
*/

let processCell cell => {
  id: cell.id,
  userValue: cell.userValue,
  computedValue: Some cell.userValue
};

let processRow row => Js.Array.map processCell row;

/* Top level function to be used as public API of this module */
let process grid => Js.Array.map processRow grid;
