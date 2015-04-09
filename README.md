# normalize-imports: Sort and align Haskell import statements

Given a file

```haskell
import Foo
import qualified Bar

import FooBar
import Baz
```
when you run `:%!normalize-imports` in Vim, you will get

```haskell
import qualified Bar
import           Foo

import           Baz
import           FooBar
```
