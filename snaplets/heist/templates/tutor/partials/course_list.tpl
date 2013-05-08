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
      <th>Highscore</th>
      <th>Bearbeitungszeit</th>
      <th>Einsendungen</th>
      <th>Beste Bewertung</th>
    </tr>
    <assignedtasks>
    <tr>
      <td><taskname /></td>
      <td><tasktype /></td>
      <td><status /></td>
      <td><highscore /></td>
      <td><timespan /></td>
      <td><submissions /></td>
      <td><bestscore /></td>
    </tr>
    </assignedtasks>
  </table> 
  <hr>
</courses>
