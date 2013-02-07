<apply template="base">
  <div class="well">

    <h2>Select a task to configure</h2>

    <br />
   
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
