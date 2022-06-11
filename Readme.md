# Duckstruct

## Functions

```
f bark(animal) {
  return animal.bark();
}
```

Gather the following information on initial analysis:

- argument list, assign algebraic types
- gather method invocations on arguments, assign duck constraints

### Motivation

- Assigning algebraic types moves the determinacy of the function call's signature to the caller

### Example

#### Step 1: Assign algebraic type

At this stage, we just want to say an animal is any type of `'a`.

```
f bark(animal: 'a) {
  return animal.bark();
}
```

Given a function with two arguments that might seem the same, we shall not reduce
them to the same algebraic type and instead leave them unique, eg. `'a` and `'b`.

```
f barkers(animal1: 'a, animal2: 'b) {
  animal1.bark();
  return animal2.bark();
}
```

#### Step 2: Assign duck constraints

If a method is called on an argument, the argument must be constrained to the method signature which is called on it.

```
f bark(animal: 'a where { bark() }) {
  return animal.bark(); // At this point, we know that
                        // this animal must have 'bark' callable on it.
                        // We can now effectively describe the type of animal
                        // as "an 'a that must be able to bark without arguments.
}
```

At this stage, we don't exactly know "what" a bark returns, however, we do have enough information to state,
"whatever type is returned from an animal.bark(), is returned from bark()". Thus, we can constrain bark's return type:

```
f bark(animal: 'a where { bark() -> 'bark }) -> 'bark {
  return animal.bark();
}
```

#### Valid barking

```
class Dog {
  bark() {
    return "woof";
  }
}

// A dog in this case is known to be a:
// 'a where { bark() -> "woof" }
// Note: we consider "woof" to be its own type, and not of type string
let dog = Dog();

// cat's type can be inferred as:
// 'b where { bark() -> 10101010 }
// Note: we consider "10101010" to be its own type, and not of type number
let cat = {
  bark: -> 10101010 // it's a weird cat idk
};

// effective type signature of bark:
// f(animal: 'a where { bark() -> 'bark }) -> 'bark
f bark(animal) {
  return animal.bark();
}

// During compilation, we can start filling in the unknowns.
// Given:
// - dog  = 'a where { bark() -> "woof" }
// - bark = f('a where { bark() -> 'b }) -> 'b
//
// When:
// bark(dog)
//
// Substitute:
// bark(dog) = f('a where { bark() -> "woof" }) -> "woof"
//
// Therefore:
// A bark of dog, or bark(dog), is a
// function that returns a "woof".
let dogBark = bark(dog); // dogBark is "woof"

let catBark = bark(cat); // catBark is 10101010
```

By using constant primitive types such as a "woof" as opposed to a string,
we can optimize the output of the code such that:

```
// no need to keep the function calls around here since we could algebraically
// reduce it to constant primitives
let dogBark = "woof"
let catBark = 10101010
```

---

This should in theory be fairly similar to the [Hindley Milner Type System](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system),
except instead of doing just type inference, we'll try to compute the values as well.