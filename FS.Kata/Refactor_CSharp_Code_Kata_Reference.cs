#!/usr/bin/env dotnet

//https://www.linkedin.com/posts/milan-jovanovic_here-are-8-tips-to-make-your-code-clean-activity-7487031571078823936-At9X?utm_source=share&utm_medium=member_desktop&rcm=ACoAADEBiPYBkFsbI9rBcOSjHffys8vFu3Ghy0k

void Process(Order? order)
{
    if (order != null)
    {
        if (order.IsVerified)
        {
            if (order.Items.Count > 0)
            {
                if (order.Items.Count > 15)
                {
                    throw new Exception(
                        "The order " + order.Id + " has too many items");
                }

                if (order.Status != "ReadyToProcess")
                {
                    throw new Exception(
                        "The order " + order.Id + " isn't ready to process");
                }

                order.IsProcessed = true;
            }
        }
    }
}

record OrderItem
{
    public required string Id { get; init; }
}

record Order
{
    public required string Id { get; init; }
    public bool IsVerified { get; init; }
    public ICollection<OrderItem> Items { get; init; } = [];
    
    public string Status { get; set; }
    public bool IsProcessed { get; set; }
}