<h3>Kurse</h3>

<courses>
  <h4>
    <a href="/courseDetails/{courseId}">
      <courseName />
    </a>
  </h4>

  <div class="row">
  <table class="table table-bordered table-condensed span6">
    <tr>
      <th class="span2">Einschreibung</th>
      <td><enrollment /></td>
      <!-- TODO: make dynamic -->
    </tr>
    <tr>
      <th>Anzahl Studenten</th>
      <td><students /> von <capacity /></td>  <!-- TODO: add capacity -->
    </tr>
  </table>
  </div>

  <h5>zugewiesene Aufgaben</h5>
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
