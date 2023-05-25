---
title: "**Study : Type of Apply function**"
subtitle: "*R apply 계열 함수 총 정리 1 ( apply / lapply / sapply / vapply )*"
author: "**JH Olivia Jeon**"
date: "`r format(Sys.Date())`"
output:
  html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
REFERENCE : 1) https://kkokkilkon.tistory.com/67

-------------------------------
#### **iris 데이터 필터링**

```{r, results = 'hide'}
iris
iris_num <- iris[1:10, 1:4]
```

```{r}
iris_num
```
랜덤으로 NA를 넣을 행/열 번호 뽑기
```{r}
set.seed(123)
idx_r <- sample(1:10, 2); idx_r
idx_c <- sample(1:4, 2) ; idx_c
```
NA 넣기
```{r}
for(i in 1:2){
  iris_num[idx_r[i], idx_c[i]] <- NA
}

iris_num
```
<hr/>
### *1. apply*
- apply(1:Input DATA, 2:행 =1; 열 =2, 3:연산자 함수)
- apply 계열 함수 중 가장 기본이 되는 함수으로, 행(Row) 또는 열(Column) 단위의 연산을 쉽게 할 수 있도록 지원하는 함수이다.

- MARGIN이라는 함수 인자에 어떤 단위로 연산을 할 것인지 1 또는 2로 표시하는데, 1이 행 단위 연산이고 2가 열 단위 연산이다.

- apply의 Input Data는 배열(Array), 매트릭스(Matrix), 데이터프레임(Data Frame) 등 모두 같은 변수형을 가진 값으로 이루어진 데이터 타입만 가능하다. 
- apply 함수의 실행 결과는 매트릭스나 벡터(Vector)로 출력된다.

<br><br>
행(row) 단위로 mean 연산
```{r}
apply(iris_num, 1, mean)
```
행(row) 단위로 mean 연산
```{r}
apply(iris_num, 1, mean)
```
열(column) 단위로 mean 연산
```{r}
apply(iris_num, 2, mean)
```
열(column) 단위로 NA 제거하고 mean 연산
```{r}
apply(iris_num, 2, mean, na.rm = T)
```
열(column) 단위로 사용자 정의 함수(function)1 연산
```{r}
apply(iris_num, 2, function(x) { x * 2 + 1 })
```
열(column) 단위로 사용자 정의 함수(function)2 연산
```{r}
apply(iris_num, 2, function(x) { median(x * 2 + 1) })
```
apply 인자로 na.rm = T 를 사용한 경우 에러 발생
```{r, error=TRUE}
apply(iris_num, 2, function(x) { median(x * 2 + 1) }, na.rm = T)
```
```{r}
apply(iris_num, 2, function(x) { median(x * 2 + 1, na.rm = T) })
```
**mean 함수 안의 인자로 na.rm = T를 써줘야 함**<br>
처음 mean 함수 연산 코드의 경우 na.rm = T 인자가 mean 함수의 인자이기 때문에 바로 apply의 인자처럼 사용할 수 있었지만, 이번 코드에서는 na.rm = T 가 function 함수의 인자는 아니기 때문에 이번 경우에는 apply의 인자처럼 na.rm = T 를 사용할 수 없다. 따라서 아래와 같이 median 함수 안의 인자로 작성해주어야 한다.

<hr/>
### *2. lapply*

list + apply를 의미하는 이름의 함수로, 실행 결과가 list 형태로 출력되는데, 리스트(list)의 인자는 length( 데이터 ) 만큼 생성된다.
데이터프레임(data frame)인 경우 length( 데이터 )의 결과는 변수의 개수(열의 개수)이고, 리스트인 경우 length( 데이터 )는 리스트 인자의 개수이다.

#### **list : R에서 사용하는 데이터 타입 중 제약사항이 거의 없는 데이터 타입**
- 데이터프레임은 모든 변수(인자)가 벡터(vector)를 가져야 하는 반면, 
- 리스트는 벡터 외에도 매트릭스(matrix), 데이터프레임(data frame) 등 어떠한 형태든 인자 안에 저장할 수 있다.
- 데이터프레임은 NA를 넣더라도 모든 변수(인자)에 들어가는 값의 수(row)가 동일해야 하지만, 리스트는 인자에 따라 값의 길이가 달라도 상관이 없다.

#### apply vs. lapply
iris_num의 열 단위 평균이 vector 형태로 출력됨
```{r}
apply(iris_num, 2, mean, na.rm = T)
```
iris_num의 열 단위 평균이 list 형태로 출력됨
```{r}
lapply(iris_num, mean, na.rm = T)
```

_<리스트를 벡터로 변환하기>_

-List 상태에서는 벡터 상태에서 보다 조작의 자유도가 떨어진다. 예를 들어 산술 연산을 한다거나 할 수 없다. 다양한 조작을 위해 리스트를 벡터로 변환할 때는 아래와 같이 unlist() 키워드를 이용하면 된다. 

Create lists.
```{r}
list1 <- list(1:5)
print(list1)

list2 <- list(10:14)
print(list2)
```

Create the lists to vectors.
```{r}
v1 <- unlist(list1)
v2 <- unlist(list2)

print(v1)
print(v2)
```

Now add the vectors
```{r}
result <- v1+v2
print(result)
```

<hr/>
### *3. sapply*

sapply 함수는 lapply 함수에서 사용자 편의성을 고려한 함수이다. sapply( , simplify = F) 인 경우 lapply( ) 와 같은 결과를 출력한다. 
**sapply 함수는 for문을 대체할 수 있는 가장 편리한 함수이기도 하다.**

아래 코드와 같이 기본적으로 sapply 함수는 연산 결과를 벡터 형태로 출력한다. 하지만 Input Data가 아래 예제와 다르게 리스트 형태라 인자간 길이가 다른 경우에는 결과 값의 인자도 길이가 서로 다르기 때문에 결과가 벡터 형태가 아닌 리스트 형태로 출력된다.


기본적으로 sapply 함수는 연산 결과를 벡터 형태로 출력한다.
```{r}
sapply(iris_num, mean, na.rm = T)
```
simplify = F이면 lapply와 동일하게 리스트 형태로 결과를 출력한다.
```{r}
sapply(iris_num, mean, na.rm = T, simplify = F)
lapply(iris_num, mean, na.rm = T)
```

<hr/>
### *4. vapply*
vapply 함수는 sapply 함수와 유사한데 추가적으로 출력되는 결과의 양식(Template)을 지정할 수 있다. 아래 코드를 보면 더 이해가 쉽다.

- sapply 함수로 fivenum 출력
```{r}
sapply(iris_num, fivenum)
```

- vapply 함수로 fivenum 출력 양식 지정하여 출력 - 출력 내용은 sapply와 동일한 형태이다
```{r}
vapply(iris_num, fivenum, c("Min." = 0, "1st Qu." = 0, "Median" = 0, "3rd Qu." = 0, "Max." = 0))
```

- sapply에서는 출력양식 지정하기가 적용되지 않는다.
```{r, error=TRUE}
sapply(iris_num, fivenum, c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))
```
