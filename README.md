<html>
<head>
</head>
  <h1>Exploratory Data Analysis of Global Democracy Data 🌎</h1>
  <p>This project aims to observe various aspects of democracy across the globe. The project can be separated into 6 sections:</p>
  <ol>
<li><strong>Democracy Indices by Continent 🌍🌎🌏</strong></li>
<li><strong>Major vs High Fluctuating Countries 📈📉</strong></li>
<li><strong>Slope Coefficients 📐</strong></li>
<li><strong>Top Countries by Democracy Index 🏆</strong></li>
<li><strong>Country Names Over Time 🕒</strong></li>
<li><strong>Leaders and Democracy Index 👑</strong></li>
</ol>
  <p>In order to better understand the project, the Analysis file should be viewed.</p>
  <h2>Background Information 📚</h2>
<p>In this project, we explore how democratic different countries are over time. We use the <strong>vdem dataset</strong>, which has data from 1789 to 2022. The dataset has <strong>27555 rows</strong> and <strong>4603 columns</strong>. Each row is a combination of a country and a year. For example, India in 2022, India in 2021, China in 2022, China in 2021, etc.</p>
<h3>Countries in the Dataset 🗺️</h3>
<p>The dataset includes countries from different periods and statuses. Some examples are:</p>
<ul>
<li>Countries from before they gained independence, such as India before 1947 🇮🇳.</li>
<li>Countries that are currently unrecognized, such as Somaliland 🇸🇴.</li>
<li>Countries that no longer exist, such as South Vietnam 🇻🇳.</li>
</ul>
<h3>Democracy Indices in the Dataset 📊</h3>
<p>The main variables we analyze are the <strong>five democracy indices</strong>. These indices measure different aspects of democracy in each country and year. They are:</p>
<ol>
<li><strong>Electoral Democracy Index 🗳️</strong>: How well the electoral processes and the administration of democracy work.</li>
<li><strong>Liberal Democracy Index 🗽</strong>: How much the rights and freedoms of the people are protected.</li>
<li><strong>Participatory Democracy Index 🙋</strong>: How much the people participate in the democratic processes.</li>
<li><strong>Deliberative Democracy Index 💬</strong>: How much the policies are deliberated over before being enacted.</li>
<li><strong>Egalitarian Democracy Index 🏳️‍🌈</strong>: How equal the groups of people are in terms of wealth, race, religion, etc.</li>
</ol>
  <h2>Democracy Indices by Continent 🌍🌎🌏</h2>
  <ul>
    <li><b>Electoral Democracy Index 🗳️:</b> Europe has the highest median Electoral Democracy Index, followed by the Americas and Oceania, while Asia and Africa have the lowest. This reflects the prevalence of democracies in Europe and the Americas, and the presence of autocracies in Asia and Africa.</li>
    <li><b>Liberal Democracy Index 🗽:</b> All continents have lower median Liberal Democracy Indices than Electoral Democracy Indices.</li>
    <li><b>Participatory Democracy Index 🙋:</b> All continents have lower median Participatory Democracy Indices than Electoral or Liberal. Africa and Asia have less decline than other continents because they have fewer democracies to begin with.</li>
    <li><b>Deliberative Democracy Index 💬:</b> Deliberative Democracy Indices have the most variation and the least gap among the continents. This implies that deliberation, which is a key feature of democracy, is not well-practiced in many democracies, and that some autocracies may still have some forms of deliberation.</li>
    <li><b>Egalitarian Democracy Index 🏳️‍🌈:</b> Europe has the highest median Egalitarian Democracy Index, because of its racial homogeneity and low economic inequality. The Americas have lower scores relative to the other indices, largelt due to their racial diversity and high economic inequality.</li>
  </ul>
<h2>Major vs High Fluctuating Countries 📈📉</h2>
  <p>This section analyzes each of the democracy scores over time, with there being a plot of "major" countries and "high fluctuating" countries. 
    Major countries were determined somewhat arbitrarily, with the US 🇺🇸, Russia 🇷🇺, China 🇨🇳, France 🇫🇷, UK 🇬🇧, and India 🇮🇳 being classified as such. High fluctuating countries were determined as the three countries for each index that had the most fluctuation over time in that democracy index.</p>
  <p>Analysis of the results can be found in the pdf</p>
  <h2>Slope Coefficients 📐</h2>
  <p>This section has a beeswarm plot showing the slopes of increase (or decrease) in Electoral Democracy index by year from 1900 to 2023. Each country was included, with the beeswarm plot segmented by continent</p>
  <p>There also does not appear to be much difference from one continent to another, with every continent having relatively similar rates of change </p>
  <h2>Top Countries by Democracy Index </h2>
  <p>This section was easily the most impressive, as it features the creation of an interactive barplot that has the top 10 most democratic nations, with the option to choose whichever year you wish and whichever democracy index you wish. On top of this, the flags of the countries are on the bars,
  and the bars are colored by continent . </p>
  A video demonstration of this shiny object can be found here: https://www.youtube.com/watch?v=OlqXibUtCqc 🎥
  <h2>Country Names Over Time 🕒</h2>
  <p>This was a fascinating observation of the prevalence of certain words in official country names over time. This required a complex set of data modifications, as no data was provided for each of the words.</p>
  <p>The analysis shows that ‘republic’ has become the most common word in country names, while ‘kingdom’ has declined. This reflects the rise of democracy and the fall of monarchy. The words ‘British’, ‘colony’, and ‘protectorate’ peaked in the early 20th century, 
    when the colonial empires collapsed . The word ‘empire’ was more popular in the late 19th century, possibly due to more countries being added to the data .</p>
  <h2>Leaders and Democracy Index 👑</h2>
  <p>This section presents some graphs that show how the Electoral Democracy Index changed under different leaders of various countries.</p>
  <p>The Analysis was done on 8 countries:</p>
  <ol>
<li><strong>United States of America 🦅</strong>: The US democracy index fluctuated depending on the political events and policies of each president. The index dropped significantly during the Bush Jr. and Trump eras, and did not recover much under Biden.</li>
<li><strong>China ㊗️</strong>: General trends to see here is the increase in democracy score under Ye Jianying and the decrease under current leader Xi Jinping.</li>
<li><strong>India 🪔</strong>: India’s democracy index peaked in the late 1990s and declined since then. The index dropped sharply during Modi’s tenure as prime minister.</li>
<li><strong>Russia/Soviet Union ❄️</strong>: Russia’s democracy score rose during the transition from the Soviet Union to the Russian Federation, and fell during Putin’s rule.</li>
<li><strong>Brazil 🧉</strong>: Brazil’s democracy score improved during Costa’s term, and remained stable for a while. The score declined during Temer’s and Bolsonaro’s presidencies.</li>
<li><strong>United Kingdom 💂</strong>: The UK’s democracy score rose during Blair’s first term, because of the Good Friday Agreement in Northern Ireland and the changes in the House of Lords.</li>
<li><strong>South Korea ☯️</strong>: South Korea seemingly has the democracy score heavily affected by who is president, as the index changes wildly depending on who the president is at a given moment in time.</li>
<li><strong>South Africa 🦁</strong>: South Africa’s democracy index soared during Mandela’s term, when the racial segregation system ended. The index declined in the 2010s, possibly due to corruption and inequality.</li>
</ol>
  <h2>Conclusions 🌟</h2>
  The analysis shows how democracy varies across countries, continents, time, and leaders, using the vdem dataset. 
  The dataset is very rich and has many more variables to explore in future analyses. The analysis reveals some interesting patterns and trends, such as the rise and fall of democracy in different regions, the changes in country names over time, and the impact of leadership on democracy.
  <h2>Author 👨‍💻</h2>
  <p>Michael Ryvin</p>
  <h2>Source 🌐</h2>
  <p>The democracy data used in this study came from the R library vdemdata, which can be found at the github repository <a href="https://github.com/vdeminstitute/vdemdata">https://github.com/vdeminstitute/vdemdata</a></p>
</body>
</html>
