---
layout: post
title: "Effective Modern C++"
date: 2015-02-28 17:30
comments: true
categories: 
---
This is a summary after reading Scott Meyer’s Effective Modern C++. As a programming language with 32 years of history, the new standard of C++11 and C++14 revitalize the whole language. As one of the mainstream PLs, numerous briliant people contribute to the renovation, including celebrity like Bartosz Milewski who set foot both in Haskell and C++. Lots of good design are added (though still with a truck of bad and legacy design tagging along). The new book discuss more practice you should adopt with new standard, and retire the old practices related to C++03. I am going to sum up a few practices and gotcha that I could still recall after finishing the whole book.

### move
move is actually a misnomer. It is not moving anything. What it does is actually using a universal reference to capture the variable, and with the correctly deduced T, it using std::remove_reference_t to remove the statically cast the lvalue to rvalue. the naming of it doesn’t involve any movement. It is the rvalue gives compilers the hint about what could be optimized. And move doesn’t necessarily mean it is cheaper than copy. You still have to check out if the implemntation for rvalue operation to see how much does it cost.

```
#include <iostream>
#include <type_traits>
#include <string>

template<typename T>
decltype(auto) my_move(T&& param)
{
    using ReturnType = std::remove_reference_t<T>&&;
    return static_cast<ReturnType>(param);
}

void p(std::string s)
{
    std::cout << s << std::endl;
}

int main()
{
    std::string s("hello world");
    p(my_move(s));
    return 0;
}
```

### noexcept
The containers in the standard library doesn’t all adopt the move semantic of any user defined type automatically with you set the compiler flag. Most of them use a conditaionl move strategy to avoid the problem of exceeption. Therefore, in order to take the benefit from move semantic. You have to somehow indicate that your function would not throw any exception. Which is noexcept keyword. But this keyword could be used as the operator to tell if the function would throw exception or not. The following is a snippet that implement a swap only does its job when the individual elemental swap doesn’t throw any exception.

```
#include <iostream>
#include <cstddef>
#include <array>
#include <algorithm>

template <class T, std::size_t N>
void my_swap(std::array<T, N>(&a), std::array<T, N>(&b)) noexcept(noexcept(std::swap(a[N], b[N])))
{
    for (int i = 0; i < N ; i++) {
        std::swap(a[i], b[i]);
    }
}

int main()
{
    std::array<int, 3> a = {1,2,3};
    std::array<int, 3> b = {4,5,6};

    my_swap(a, b);

    for (auto&&i: a) {
        std::cout << i << " ";
    }

    return 0;
}
```

### lambda
The best practice about lambda is avoiding the default capture mode. With C++14 it comes with [a = b] init capture syntax. It eases the pain of unique_ptr moving and POT copy capture.

```
#include <vector>
#include <functional>

int main()
{
    using FilterContainer = std::vector<std::function<bool(int)> >;

    FilterContainer filters;
    int divisor = 2;
    filters.emplace_back([divisor = divisor](int value) { return value % divisor == 0; });

    return 0;
}
```

### perfect forwarding
Perfect forwarding in 99% of the scenarios would be perfectly functional. Some exceptions that it doesn’t work is that the constructor has different meaning with the argument list (a, b) and initializer list {a, b}. std::vector is one of them. std::vector a(2, 3) means create a vector with two 3. but not a vector with its content [2, 3]

```
#include <vector>
#include <iostream>

template<typename... Ts>
std::vector<int> fwd(Ts&&... params)
{
    return std::vector<int>(std::forward<Ts>(params)...);
}

template<typename... Ts>
std::vector<int> fwd2(Ts&&... params)
{
    return std::vector<int>{std::forward<Ts>(params)...};
}

int main()
{
    auto&& v = fwd(2, 3);
    auto&& v2 = fwd2(2, 3);

    for (auto&& i: v) {
        std::cout << i << " ";
    }
    std::cout << std::endl;

    for (auto&& i: v2) {
        std::cout << i << " ";
    }
    std::cout << std::endl;

    return 0;
}
```

### universal reference
Sometimes universal referece would be too aggressive and hinder the function you would really like to match. The answer is to use enable_if to let it only match under certain constraints.

```
#include <iostream>

class Foo {
public:
    template <typename T>
    explicit Foo(T&& f) { std::cout << "universal ref" << std::endl; }

    Foo(const Foo& f) { std::cout << "copy constructor" << std::endl; }
    Foo(Foo&& f) { std::cout << "move constructor" << std::endl; }
};

int main() {
    Foo a(1);
    const Foo ac(1);

    auto b(a);
    auto bc(ac);

    return 0;
}
```

### emplace_back
emplace_back is a good way to save the operation cost, but it is not a explixir and could results to different meaning. Like std::regex. push_back a null_ptr would make compiler whining, but compiler would happily accept null_ptr if you call emplace_back, and blows up at runtime.

```
#include <vector>
#include <regex>

int main()
{
    std::vector<std::regex> regexes;
    regexes.emplace_back(nullptr);      //compile
    regexes.push_back(nullptr);         //compile error

    return 0;
}
```

### Epilogue
It’s amazing that C++ could still have great advancement in recent years, with all of its love and hate. The cost is that the language has been so complicated that few people could fully grasp. It’s also an interesting observation where that most of the interviews you met in an intervew who proclaim that they are well-versed C++ programmer are more than fifty percent likely that they are not. However, those who expresses their lack of confidence in knowing the language actually the group who are more competitive.
