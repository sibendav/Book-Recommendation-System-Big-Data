SELECT count(*) FROM nextbook4u.`bx-book-ratings`;
SELECT count(*) FROM nextbook4u.`bx-users`;
SELECT count(*) FROM nextbook4u.`bx-books`;

/*num of users rated n times */
select T.num_ratings, count(*) as num_users
from
(select nextbook4u.`bx-users`.`User-ID` as user_id, count(*) as num_ratings
from nextbook4u.`bx-book-ratings` natural join nextbook4u.`bx-users`
group by nextbook4u.`bx-users`.`User-ID`) T
group by T.num_ratings
order by T.num_ratings;

/*num of books rated n times */
select T.num_ratings, count(*) as num_books
from
(select nextbook4u.`bx-books`.`ISBN` as book_id, count(*) as num_ratings
from nextbook4u.`bx-book-ratings` natural join nextbook4u.`bx-books`
group by nextbook4u.`bx-books`.`ISBN`) T
group by T.num_ratings
order by T.num_ratings;

/*top 10 rated books*/
select nextbook4u.`bx-books`.`Book-Title`, T.num_ratings
from nextbook4u.`bx-books` natural join
(select nextbook4u.`bx-books`.`ISBN`, count(*) as num_ratings
from nextbook4u.`bx-book-ratings` natural join nextbook4u.`bx-books`
group by nextbook4u.`bx-books`.`ISBN`) T
order by T.num_ratings Desc
limit 10;

/*top 10 active users */
select *
from 
(select nextbook4u.`bx-users`.`User-ID` as user_id, count(*) as num_ratings
from nextbook4u.`bx-book-ratings` natural join nextbook4u.`bx-users`
group by nextbook4u.`bx-users`.`User-ID`) T
order by T.num_ratings desc
limit 10;

