---
sidebar_position: 3
---

# Syntax Basics

Duckstruct syntax is expression-oriented and keeps declarations lightweight.

## Variables

```duckstruct
let x = 1
let greeting = "hello"
```

## Functions

```duckstruct
f add(a, b) {
  a + b
}
```

## Object access

```duckstruct
let point = { x: 3, y: 4 }
point.x
```

## Conditionals

```duckstruct
if x > 0 {
  "positive"
} else {
  "not positive"
}
```

## Loops

```duckstruct
for item in items {
  item
}
```
