<groups>
  <h3><courseName /></h3>
  <ul>
    <li>Prüfungszulassung mit <passCriteria />% bestandener Belegaufgaben</li>
  </ul>

  <h4>Übungsgruppe</h4>
  <ul>
    <li>
      <groupDescription />
      (<a href="/student/signout/${groupId}">austragen</a>)
    </li>
  </ul>

  <h4>Belege</h4>
  <table class="table table-bordered table-condensed table-striped">
    <tr>
      <th>Name</th>
      <th>Art</th>
      <th>Bearbeitungszeit</th>
      <th>Einsendungen</th>
      <th>Beste Bewertung</th>
      <th>Meine beste Bewertung</th>
      <th></th>
    </tr>
    <assignments>
    <tr>
      <td><description /></td>
      <td><status /></td>
      <td><submissionTime /></td>
      <td><submissions /></td>
      <td><bestscore /></td>
      <td><mybestscore /></td>
      <td>
        <a href="/student/${studentId}/solve/${taskInstanceId}"
           role="button"
           class="btn btn-primary">Jetzt lösen</a>
      </td>
    </tr>
    </assignments>
  </table> 
  <hr>
</groups>
