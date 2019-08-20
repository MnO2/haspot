---
layout: post
title: "Travel Planning with Trello"
date: 2019-08-20 18:55 
comments: true
categories: 
---
As a frequent traveller who has been to 55+ countries/regions, I know that travel planning is a time consuming process. I enjoyed the process pretty much as it would get me to know the destination and the culture more. Being to the destination is only small part of it. However, planning it with the right tool definitely would make your life much easier. 

I was quite used to put everything into one Google Doc. I have been using the template I created for myself all over these years. I would revise the template from the experience from the journey to make sure the template is thorough and it could handle 90% of the scenario. Whenever I am about to plan my next trip, I'll just copy from my template and dump the materials and the links to the wikitravel and blog posts into the Google Doc and use it as a draft to plan my trip.

Until I find a significant better way to do it. It's part of the movement I set for myself to make most of the things in my life to be managed by Trello. I would centralize the notification/update by integrating the crawler and Trello API. But soon I found that Trello is especially suitable for travel planning. This blog post is not sponsored by Trello but I whole-heartedly think so.

Travel planning is a classic type of tasks that has the following properties:

1. Start from a simple idea and expanded into details and converge to a final plan
2. The final outcome is a detailed itinerary with contingency plan.
3. Itinerary alone is not enough. Analytical reporting is required since you need budgeting.

Therefore at least to me, a good software for travel planning need to have the following features:

1. A pin board to collect the materials you are gonna to read.
2. Easy to re-organize and label items to suit your need or your mind map looks like.
3. Rich media supports
4. It provides APIs so that you could export and analyze it with a proper tooling.
5. Synchronization and Offline access.

And Trello just matches all of them to me as the time of Aug 2019, and this is how I use it.

At the brainstorming phase, I would install the Send-to-Trello button to my Firefox. Whenever I looked up a relevant blog post and I don't have time to read it full, I would use Send-to-Trello button to save it later. Usually I would create a List in Trello and name it as `Lead`.

![](/images/2019-08-20/send_to_trello.png)

And I would read them and create the digested steps into Cards and organize them into days. This is an iterative process, I would first focus on the ballpark schedule so that I could put everything into the my scheduled numbers of days, and make sure the big transporation items are possible to tickets in the budget. In the later iterations, I would add more detailed steps including the ticket and transfer  information etc, so that I could just follow the instructions when I am on the road. The Card design just makes this iterative process easy and intuitive.

![](/images/2019-08-20/trello_cards.png)

With the Adds-on, Trello could even show you the Map View of your cards, it's especially useful when you are planning the places you would like to visit in the city. You could see the obvious clusters for the markers on the map, and put them in the same day.

![](/images/2019-08-20/trello_map_view.png)

Also I would leverage on the Custom Fields adds-on to create extra field to document the cost and duration of the transportation. These are for further analysis at the later phase. 

After finishing the planning and make sure I was correctly labeling the cards. I would run a script against Trello API to generate the Google Sheets budget spreadsheet.
 
```
#!/usr/bin/env ruby

require "google_drive"
require 'trello'
require 'time'
require 'json'

spreadsheet_id = '<Your Spread Sheet ID>'
target_currency = 'TWD'

config_file = File.read("config/trello.json")
config_for_trello = JSON.parse(config_file)

Trello.configure do |config|
  config.developer_public_key = config_for_trello['developer_public_key'] 
  config.member_token = config_for_trello['member_token'] 
end

cards = {}

Trello::Board.find('<Your Board ID>').lists.each do |list|
  Trello::List.find(list.id).cards.each do |card|
    cards[card.id] = { :name => card.name }
    card.custom_field_items.each do |item|
      pp item.custom_field_id
      if item.custom_field_id == '5d5978f014ab2f17fcb6a2cd'
        if not item.value['number'].nil?
          cards[item.model_id][:cost] = item.value['number']
        end
      elsif item.custom_field_id == '5d59795638dd318d1a53471a'
        if not item.option_value().nil?
          cards[item.model_id][:currency] = item.option_value()['text']
        end
      end
    end
  end
end

pp cards

row = 1
session = GoogleDrive::Session.from_config("config/google.json")
ws = session.spreadsheet_by_key(spreadsheet_id).worksheets[0]

cards.each do |card_id, card|
  if not card[:cost].nil? and not card[:currency].nil?
    ws[row, 1] = card[:name]
    ws[row, 2] = card[:cost]
    ws[row, 3] = card[:currency]
    ws[row, 4] = target_currency

    row += 1
  end
end
ws.save
```

And here is what it looks like in Google Sheets.

![](/images/2019-08-20/budget_table.png)

I used it to plan my trip to Kyoto in the upcoming month, and it is so much better than what I was doing by Google Doc, reducing the big share of time to do copy pasting and adjusting the format. I would use this method to plan my trip in the future.
