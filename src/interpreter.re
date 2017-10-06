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

  Simplified first approach:

    * Iterate over the original list until all cells are resolved
    * How do we ensure it terminates? Use reduce rather than map?
*/


let interpretExp value grid => {
  Js.log value;
  Some value
};

/*
   Logic for iterating over all the cells, still needs a queue to allow referencing
   other cells in the grid
 */
let isPrefixedWithEquals str => (String.length str > 0) && (switch (String.get str 0) {
  | '=' => true
  | _ => false
});
let isUnresolved cell => cell.computedValue == None && (isPrefixedWithEquals cell.userValue);
let isEmpty cell => cell.userValue == "";
let isResolved cell => cell.computedValue != None;

let processCell grid cell => switch (cell) {
  | cell when (isUnresolved cell) => 
    {
      id: cell.id,
      userValue: cell.userValue,
      computedValue: (interpretExp cell.userValue grid)
    }
  | cell when (isEmpty cell || isResolved cell) => 
    {
      id: cell.id,
      userValue: cell.userValue,
      computedValue: Some cell.userValue
    }
  | cell => 
    {
      id: cell.id,
      userValue: cell.userValue,
      computedValue: Some cell.userValue
    }
};

/* Top level function to be used as public API of this module */
let process (grid : Cell.grid) : Cell.grid => Js.Array.map (fun row => Js.Array.map (processCell grid) row) grid;
