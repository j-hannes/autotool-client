<apply template="/tutor/base">
  <h2>Aufgabe zur Konfiguration auswählen</h2>

 <div class="well"> 
    <bind tag="category">
      <span class="category"><categoryName /></span>
      <ul>
        <subTrees>
          <li><element /></li>
        </subTrees>
      </ul>
    </bind>

    <bind tag="task">
      <a href="/task/configure/${taskName}"><taskName /></a>
    </bind>
    
    <ul id="task-types">
      <taskTrees>
        <li>
          <element />
        </li>
      </taskTrees>
    </ul>
  </div>
</apply>
