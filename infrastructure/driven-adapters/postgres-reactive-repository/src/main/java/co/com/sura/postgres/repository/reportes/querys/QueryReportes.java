package co.com.sura.postgres.repository.reportes.querys;

public enum QueryReportes {
    FIND_REPORTE_ANUAL_BY_YEAR_REGIONAL("SELECT CAST(EXTRACT(MONTH FROM fecha_turno) AS TEXT) AS mes, " +
            "AVG(capacidad_actual)  AS \"capacidadPromedio\", " +
            "SUM(horas_completadas) AS \"totalHorasAtencionesCompletadas\", " +
            "SUM(citas_completadas) AS \"totalCitasCompletadas\", " +
            "SUM(citas_canceladas)  AS \"totalCitasCanceladas\", " +
            "SUM(total_remisiones)  AS \"totalRemisiones\", " +
            "SUM(total_novedades)   AS \"totalNovedades\", " +

            "CASE WHEN SUM(horas_asignadas) = 0 THEN 0 ELSE (SUM(horas_completadas) / " +
            "NULLIF(SUM(horas_asignadas), 0)) * 100 END AS \"cumplimientoCitasPromedio\""+

            "FROM reportes_turno " +
            "WHERE EXTRACT(YEAR FROM fecha_turno)= $1 AND id_regional = $2 " +
            "GROUP BY mes"),

    FIND_REPORTE_MENSUAL_BY_YEAR_REGIONAL("SELECT CAST(EXTRACT(DAY FROM fecha_turno) AS TEXT) AS dia, " +
            "AVG(capacidad_actual)  AS \"capacidadPromedio\", " +
            "SUM(horas_completadas) AS \"totalHorasAtencionesCompletadas\", " +
            "SUM(citas_completadas) AS \"totalCitasCompletadas\", " +
            "SUM(citas_canceladas)  AS \"totalCitasCanceladas\", " +
            "SUM(total_remisiones)  AS \"totalRemisiones\", " +
            "SUM(total_novedades)   AS \"totalNovedades\", " +

            "CASE WHEN SUM(horas_completadas) = 0 THEN 0 ELSE (SUM(horas_completadas) / " +
            "NULLIF(SUM(horas_asignadas), 0)) * 100 END AS \"cumplimientoCitasPromedio\""+

            "FROM reportes_turno " +
            "WHERE EXTRACT(MONTH FROM fecha_turno)= $1 AND " +
            " EXTRACT(YEAR FROM fecha_turno)= $2 " +
            " AND id_regional = $3 " +
            "GROUP BY dia;"),

    FIND_MOTIVOS_CANCELACION_ANUAL("SELECT " +
            "  CAST(EXTRACT(MONTH FROM public.registro_cancelacion_citas.fecha_cancelacion) AS TEXT) AS mes, " +
            "  public.motivo_cancelacion.descripcion, " +
            "  COUNT(public.registro_cancelacion_citas.id_cita) AS \"cantidadCancelaciones\" " +
            "FROM public.registro_cancelacion_citas " +
            "INNER JOIN public.motivo_cancelacion ON " +
            "public.motivo_cancelacion.id = public.registro_cancelacion_citas.id_motivo_cancelacion " +
            "INNER JOIN public.citas ON " +
            "public.registro_cancelacion_citas.id_cita = public.citas.id_cita AND public.citas.id_regional = $1 " +
            "WHERE EXTRACT(YEAR FROM public.registro_cancelacion_citas.fecha_cancelacion)= $2 " +
            "GROUP BY public.motivo_cancelacion.descripcion,public.registro_cancelacion_citas.fecha_cancelacion;"),

    FIND_MOTIVOS_CANCELACION_MENSUAL("SELECT " +
            "    CAST(EXTRACT(DAY FROM public.registro_cancelacion_citas.fecha_cancelacion) AS TEXT) AS dia, " +
            "    public.motivo_cancelacion.descripcion, " +
            "    COUNT(public.registro_cancelacion_citas.id_cita) AS \"cantidadCancelaciones\" " +
            "FROM public.registro_cancelacion_citas " +
            "INNER JOIN public.motivo_cancelacion ON " +
            "public.motivo_cancelacion.id = public.registro_cancelacion_citas.id_motivo_cancelacion " +
            "INNER JOIN public.citas ON " +
            "public.registro_cancelacion_citas.id_cita = public.citas.id_cita AND public.citas.id_regional = $1 " +
            "WHERE   EXTRACT(MONTH FROM public.registro_cancelacion_citas.fecha_cancelacion)= $2 AND " +
            "   EXTRACT(YEAR FROM public.registro_cancelacion_citas.fecha_cancelacion)= $3 " +
            "GROUP BY public.motivo_cancelacion.descripcion,public.registro_cancelacion_citas.fecha_cancelacion;");

    private final String query;

    QueryReportes(String query) {
        this.query = query;
    }
    public String getQuery() {
        return query;
    }


}
