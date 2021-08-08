fun HandleBuy(ProductName : string, Amount : int, Price : real) = 
    val writestream = TextIO.openAppend "output.asm"; 
    val content = ["### BUY ", ProductName, " ###"];
    TextIO.output (writestream, content);
    

    (*== or ==

    TextIO.output (writestream, "### BUY ");
    TextIO.output (writestream, ProductName); 
    TextIO.output (writestream, " ###");

    val total = Amount * Price;

    == or == *)

    val total = real(Amount) * Price;

    TextIO.output (writestream, total);
    TextIO.closeOut writestream;

