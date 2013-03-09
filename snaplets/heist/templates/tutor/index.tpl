<apply template="base">
  <div class="well">
    <h4>Kurse</h4>

    <br>

    <form>
      <table class="table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Semester</th>
            <th>Einschreibung Start</th>
            <th>Einschreibung Ende</th>
            <th>Eingeschrieben</th>
            <th>Loeschen</th>
          </tr>
        </thead>
        <tbody>
          <courses>
            <tr>
              <td>
                <a href="#courseDetailsModal_${courseId}">
                  <courseName />
                </a>
              </td>
              <td><courseSemester /></td>
              <td><enrollmentBegin /></td>
              <td><enrollmentEnd /></td>
              <td><students /></td>
              <td>
                <a href="#" class="btn btn-danger">
                  <i class="icon-trash"></i>
                </a>
              </td>
            </tr>
          </courses>
        </tbody>
      </table>
    </form>
  </div>

  <div class="well">
    <h4>Aufgabenkonfigurationen</h4>

    <br>

    <form>
      <table class="table">
        <thead>
          <tr>
            <th>Name</th>
            <th>Aufgabentyp</th>
            <th>Erstellt</th>
            <th>Zugewiesen</th>
            <!--<th>Zuweisen</th>-->
            <th>Loeschen</th>
          </tr>
        </thead>
        <tbody>
          <taskConfigs>
            <tr>
              <td>
                <a href="#taskConfigDetailsModal_${taskConfigId}">
                  <taskConfigName />
                </a>
              </td>
              <td><taskType /></td>
              <td><dateCreated /></td>
              <td><assignments /></td>
              <!--<td>
                <div class="input-append">
                  <select><option /></select>
                  <button class="btn">
                    <i class="icon-plus"></i>
                  </button>
                </div>
              </td>-->
              <td>
                <a href="#" class="btn btn-danger">
                  <i class="icon-trash"></i>
                </a>
              </td>
            </tr>
          </taskConfigs>
        </tbody>
      </table>
    </form>

  </div>
</apply>
