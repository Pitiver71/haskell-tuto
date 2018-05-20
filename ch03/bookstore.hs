-- exercices from Chapter 3 of the book Real World Haskell
type CustomerID = Int
type ReviewBody = String
type BookRecord = (BookInfo, BookReview)
type CardHolder = String
type CardNumber = String
type Address = [String]

data BookInfo = Book Int String [String] deriving (Show, Eq)
data MagazineInfo = Magazine Int String [String] deriving (Show)
data BookReview = BookReview BookInfo CustomerID ReviewBody deriving (Show)
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
data Customer = Customer {
  customerID      :: CustomerID,
  customerName    :: String,
  customerAddress :: Address
} deriving (Show, Eq)


bookID (Book id _ _) = id
bookTitle (Book _ title _) = title
bookAuthors (Book _ _ authors) = authors

myInfo = Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]
