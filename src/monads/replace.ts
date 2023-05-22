// Replace item at position Offset in an array of types with the type Item.
// This is done when we identify that a function type "breaks" the chain of composition.
// $Draft is a local variable used in the recursion to accumulate the elements until the replacement.
// We check that the length of $Draft === the desired position with "$Draft["length"] extends Offset"
// because we know Offset is a number - the only way to extend the type 3 for example is to be 3.
// Else we recurse with the last item $Item pushed into $Draft and $Before containing all but the last item.
export type Replace<
  T extends unknown[],
  Offset extends number,
  Item,
  $Draft extends unknown[] = []
> = $Draft["length"] extends Offset
  ? $Draft extends [any, ...infer $After]
    ? [...T, Item, ...$After]
    : never
  : T extends [...infer $Before, infer $Item]
  ? Replace<$Before, Offset, Item, [$Item, ...$Draft]>
  : never;

// Example: the type asdf is [1, 2, 3, 4, 5, 6, "hey", 8, 9]
type asdf = Replace<[1, 2, 3, 4, 5, 6, 7, 8, 9], 3, "hey">;
