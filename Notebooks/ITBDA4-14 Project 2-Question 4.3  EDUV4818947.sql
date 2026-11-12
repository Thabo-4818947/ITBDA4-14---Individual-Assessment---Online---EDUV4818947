SELECT author, COUNT(*) AS quote_count
FROM public."Quotes_clean"
GROUP BY author
ORDER BY quote_count DESC, author;

SELECT author, word_count, text
FROM public."Quotes_clean"
WHERE word_count > 15
ORDER BY word_count DESC;