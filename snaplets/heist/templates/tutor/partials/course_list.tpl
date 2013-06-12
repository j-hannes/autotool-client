<courses>
  <h3>
    <!--<a href="/courseDetails/{courseId}">-->
      <courseName />
    <!--</a>-->
  </h3>

  <ul>
    <li>Einschreibung <enrollment /></li>
    <li>Prüfungszulassung mit <passCriteria />% bestandener Belegaufgaben</li>
    <li>Zulassungsquote liegt derzeit bei <passRate />%</li>
  </ul>

  <h4>Belege</h4>
  <table class="table table-bordered table-condensed table-striped">
    <tr>
      <th>Name</th>
      <th>Aufgabentyp</th>
      <th>Art</th>
      <th>Bearbeitungszeit</th>
      <th>Einsendungen</th>
      <th>Beste Bewertung</th>
      <th>Bewertungsreihenfolge</th>
    </tr>
    <assignedTasks>
    <tr>
      <td><taskName /></td>
      <td><taskType /></td>
      <td><status /></td>
      <td><timespan /></td>
      <td><submissions /></td>
      <td><bestscore /></td>
      <td><scoringOrder /></td>
    </tr>
    </assignedTasks>
  </table> 
  <hr>
</courses>
