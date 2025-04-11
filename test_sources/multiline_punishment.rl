print (
  ( // Outer group
    ( // First chain, math and ternary
      (
        (false ? -1 : 2 + 3 ^ 2), // 2 + 9 = 11
        (true ? (5 * (1 + 1)) : 0), // 10
        (false ? 100 : (true ? 50 : 25)) // 50
      ), // Chain: 11, 10, 50 → 50
      (
        ("discard", "me", (3 + 4 * 2)), // 11
        (-2 * 3 + 4), // -2
        (true ? (false ? 8 : 7) : 6) // 7
      ) // Chain: 11, -2, 7 → 7
    ), // Multiply: 50 * 7 = 350
    ( // Second chain
      (
        (
          (false ? (1 + 2) : (3 + 4)) // 7
        ),
        (true ? (false ? 99 : (2 ^ 3)) : 0), // 8
        (false ? 0 : 1) // 1
      ), // Chain: 7, 8, 1 → 1
      ( // Nested chain to test discard
        ("x", "y", "z", (false ? 1000 : 2000)) // 2000
      )
    ) // Multiply: 1 * 2000 = 2000
  ), // Outer comma: (350, 2000) → 2000
  ( // Third main group
    (3 ^ (1 + 1 + 1 + 1)), // 3 ^ 4 = 81
    (false ? 9 : (8 * 2)), // 16
    ("last discard", (10 - 3)) // 7
  ), // Chain: 81, 16, 7 → 7
  ((("a"), ("b"), ("c"), (4 * 3))) // 12
);
//Expected answer = 12
