% Лабораторная работа № 7 «Основы программирования на Рефале»
% 11 декабря 2025 г.
% Андрей Кабанов, ИУ9-11М

# Цель работы
## UNIX-утилита

Требуется написать аналог UNIX-утилиты, указанной в индивидуальном варианте.

По соглашению, принятому в UNIX-среде, программа последовательно обрабатывает имена 
файлов, указанных в командной строке, результат обработки выводит на stdout. 
Если имена файлов отсутствуют, программа должна читать stdin.

Сообщения об ошибках должны выводиться на stderr (файловый дескриптор 0 для функции 
Putout) и завершаться ненулевым кодом возврата.

Следует иметь ввиду, что встроенная функция Arg в Рефале-5 игнорирует аргументы, 
начинающиеся на знак «минус».

## Символьные вычисления

В индивидуальном варианте описан абстрактный синтаксис некоторого языка и функция, 
которая выполняет преобразование программы на этом языке.

Требуется описать абстрактный синтаксис при помощи грамматики типов с использованием 
синтаксиса, описанного в лекции и написать функцию, работающую с данными 
указанного типа.

Входные данные должны загружаться при помощи функции LoadExpr, результат должен 
распечатываться на stdout.

# Индивидуальный вариант
## UNIX-утилита
Подмножество функциональности программы sed, используемое для замены подстрок.
```
sed s/‹строка›/‹замена›/ ‹имена файлов›…
sed s/‹строка›/‹замена›/g ‹имена файлов›…
```
Если указан ключ «g», выполняются замены всех вхождений ‹строки› на ‹замену›, 
если не указан — только первое вхождение в каждой строке.

## Символьная функция
Регулярное выражение описано следующим абстрактным синтаксисом:

`RegEx → ε | SYMBOL | RegEx ⋃ RegEx | RegEx · RegEx | RegEx*`
Здесь `⋃` — объединение, `·` — конкатенация.

# Реализация UNIX-утилиты

```
*$FROM LibraryEx
$EXTERN ArgList;
$EXTERN LoadFile;
$EXTERN SaveFile;
$ENTRY Go {
    , <IsStd<Lenw <ArgList>>>: '+' = <Prout <ProcessFiles <ParseArgs<ArgList>>>>;
    , <IsStd<Lenw <ArgList>>>: '0' = <ProcessStdin (<Card>) <ParseArgs<ArgList>>>;
    e.Any = <Putout 0 'Bad args number'> <Exit 1>;
}
TToE {
    (e.Expr) = e.Expr
}
TsToE {
    (e.Expr) = e.Expr ;
    (e.Expr) e.Else = e.Expr '\n' <TsToE e.Else>;
    =;
}
IsStd {
    s.NUM e.Expr = <Compare s.NUM 2>
}

ParseArgs{
  t.trash ( 's' '/' e.If '/' e.Of '/' ) e.filename = (e.If) (e.Of) e.filename;
  t.trash ( 's' '/' e.If '/' e.Of '/g' ) e.filename = 'g' (e.If) (e.Of) e.filename;
  e.Any = <Putout 0 'Bad args'> <Prout e.Any> <Exit 1>;
}

ProcessStdin{
  (e.Some 0) e.Params = <Prout<TToE <Replace e.Params (e.Some)>> '\n'>;
  (e.Some) e.Params = <Prout<TToE <Replace e.Params (e.Some)>>> 
                      <ProcessStdin (<Card>) e.Params>;
}

ProcessFiles {
    (e.If) (e.Of) (e.File) = <TsToE <Replace (e.If) (e.Of) <LoadFile e.File>>>;
    'g' (e.If) (e.Of) (e.File) = <TsToE <Replace 'g' (e.If) (e.Of) <LoadFile e.File>>>;
    (e.If) (e.Of) (e.File) e.Rest = <TsToE <Replace (e.If) (e.Of) <LoadFile e.File>>> 
                                    '\n' <ProcessFiles (e.If) (e.Of) e.Rest>;
    'g' (e.If) (e.Of) (e.File) e.Rest = <TsToE <Replace 'g' (e.If) (e.Of)  
                                        <LoadFile e.File>>> 
                                        '\n' <ProcessFiles 'g' (e.If) (e.Of) e.Rest>;
    
    'g' (e.If) (e.Of) = ;
    (e.If) (e.Of) = ;
}

Replace {
    (e.If) (e.Of) (e.Begin e.If e.End) e.Rest = (e.Begin e.Of e.End) 
                                                <Replace (e.If) (e.Of) e.Rest>;
    (e.If) (e.Of) (e.Begin) e.Rest = (e.Begin) <Replace (e.If) (e.Of) e.Rest>;
    (e.If) (e.Of) = ;
    'g' (e.If) (e.Of) (e.Begin e.If e.End) e.Rest = (e.Begin e.Of 
                                                    <ReplaceAll (e.If) (e.Of) (e.End)>) 
                                                    <Replace 'g' (e.If) (e.Of) e.Rest>;
    'g' (e.If) (e.Of) (e.End) e.Rest = (e.End) <Replace 'g' (e.If) (e.Of) e.Rest>;
    'g' (e.If) (e.Of) = ;
}
ReplaceAll {
    (e.If) (e.Of) (e.Begin e.If e.End) = e.Begin e.Of 
                                        <ReplaceAll (e.If) (e.Of) (e.End)>;
    (e.If) (e.Of) (e.Begin) = e.Begin;
}
```

# Тестирование UNIX-утилиты

```
andry@MagicBook-X:~/Work/FP_BMSTU/lab7$ ./main s/gog/dog/g 1.txt
dog dog sdfjfj
dog dog dog
dog
```

# Реализация символьных преобразований

```
*$FROM LibraryEx
$EXTERN LoadExpr;

$ENTRY Go {
    = <Prout <AsString <LoadExpr <Arg 1>>>>;
}

/*
t.RegEx ::= Epsilon                 - пустая строка
           | (Symbol s.CHAR)        - символ
           | (Union t.RegEx t.RegEx) - объединение
           | (Concat t.RegEx t.RegEx) - конкатенация
           | (Star t.RegEx)         - замыкание
*/
AsString {
    e.Expr = <ToString e.Expr 0>;
}


ToString {
    Epsilon e.Ctx = "";
    
    (Symbol s.CHAR) e.Ctx = <Escape s.CHAR>;
    
    (Union t.RegEx1 t.RegEx2) e.Ctx, <Compare e.Ctx 1>: '+' = 
        '(' <ToString t.RegEx1 1> '|' <ToString t.RegEx2 1> ')';
    (Union t.RegEx1 t.RegEx2) e.Ctx = 
        <ToString t.RegEx1 1> '|' <ToString t.RegEx2 1>;

    (Concat t.RegEx1 t.RegEx2) e.Ctx, <Compare e.Ctx 2>: '+' = 
        '(' <ToString t.RegEx1 2> <ToString t.RegEx2 2> ')';
    (Concat t.RegEx1 t.RegEx2) e.Ctx = 
        <ToString t.RegEx1 2> <ToString t.RegEx2 2>;
    
    (Star t.RegEx1) e.Ctx = 
        <AddStar <ToString t.RegEx1 3>>;
}

AddStar {
    s.CHAR = s.CHAR '*';
    
    '(' e.Inner ')' = '(' e.Inner ')' '*';
    
    s.Expr = '(' s.Expr ')' '*';
}

Escape {
    '|' = "\\|";
    '*' = "\\*";
    '(' = "\\(";
    ')' = "\\)";
    '\\' = "\\\\";
    s.CHAR = s.CHAR;
}
```

# Тестирование символьных преобразований

```
andry@MagicBook-X:~/Work/FP_BMSTU/lab7$ ./master 2.txt
a*b(c|d)*|e
```

# Вывод
Во время выполнения данной лабораторной работы студентом были приобретены навыки 
программирования на языке Refal. Разработанные в ходе данной лабораторной работы 
программы демонстрируют уникальные присущие Refal механизмы парсинга и 
работы с последовательностями символов.