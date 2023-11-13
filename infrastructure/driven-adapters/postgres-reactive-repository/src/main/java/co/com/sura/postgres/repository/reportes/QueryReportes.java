package co.com.sura.postgres.repository.reportes;

public enum QueryReportes {
    FIND_REPORTE_ANUAL_BY_YEAR_REGIONAL("SELECT " +
            "CAST(EXTRACT(MONTH FROM fecha_turno) AS TEXT) AS mes, " +
            "AVG(capacidad_actual)  AS \"capacidadPromedio\", " +
            "SUM(horas_completadas) AS \"totalHorasAtencionesCompletadas\", " +
            "SUM(citas_completadas) AS \"totalCitasCompletadas\", " +
            "SUM(citas_canceladas)  AS \"totalCitasCanceladas\", " +
            "(SUM(horas_completadas)/(SUM(horas_asignadas))*100) as \"cumplimientoCitasPromedio\" "+
            "FROM reportes_turno " +
            "WHERE EXTRACT(YEAR FROM fecha_turno)= $1 AND id_regional = $2 " +
            "GROUP BY mes"),

    FIND_REPORTE_MENSUAL_BY_YEAR_REGIONAL("SELECT " +
            "CAST(EXTRACT(DAY FROM fecha_turno) AS TEXT) AS dia, " +
            "AVG(capacidad_actual) as \"capacidadPromedio\", " +
            "CAST(SUM(horas_completadas) AS INTEGER) as \"totalHorasAtencionesCompletadas\", " +
            "CAST(SUM(citas_completadas) AS INTEGER) as \"totalCitasCompletadas\", " +
            "CAST(SUM(citas_canceladas) AS INTEGER) as \"totalCitasCanceladas\", " +
            "(SUM(horas_completadas)/(SUM(horas_asignadas))*100) as \"cumplimientoCitasPromedio\" " +
            "FROM reportes_turno " +
            "WHERE EXTRACT(MONTH FROM fecha_turno)= $1 AND " +
            " EXTRACT(YEAR FROM fecha_turno)= $2 " +
            " AND id_regional = $3 " +
            "GROUP BY dia;");

    private final String query;

    QueryReportes(String query) {
        this.query = query;
    }
    public String getQuery() {
        return query;
    }


}
