package co.com.sura.postgres.reportes.querys;

public class QueryReportes {
   public static final String FIND_REPORTE_ANUAL_BY_YEAR_REGIONAL = "SELECT EXTRACT(MONTH FROM fecha_turno) AS mes, " +
            "AVG(capacidad_actual)  AS \"capacidadPromedio\", " +
            "SUM(horas_completadas) AS \"totalHorasAtencionesCompletadas\", " +
            "SUM(citas_completadas) AS \"totalCitasCompletadas\", " +
            "SUM(citas_canceladas)  AS \"totalCitasCanceladas\", " +
            "SUM(total_remisiones)  AS \"totalRemisiones\", " +
            "SUM(total_novedades)   AS \"totalNovedades\", " +

           "SUM(citas_completadas)::float / NULLIF(SUM(citas_asignadas),0)*100 AS \"cumplimientoCitasPromedio\"" +

            "FROM reportes_turno " +
            "WHERE EXTRACT(YEAR FROM fecha_turno)= $1 AND id_regional = $2 " +
            "GROUP BY mes " +
            "ORDER BY mes";

    public static final String  FIND_REPORTE_MENSUAL_BY_YEAR_REGIONAL ="SELECT EXTRACT(DAY FROM fecha_turno) AS dia, " +
            "AVG(capacidad_actual)  AS \"capacidadPromedio\", " +
            "SUM(horas_completadas) AS \"totalHorasAtencionesCompletadas\", " +
            "SUM(citas_completadas) AS \"totalCitasCompletadas\", " +
            "SUM(citas_canceladas)  AS \"totalCitasCanceladas\", " +
            "SUM(total_remisiones)  AS \"totalRemisiones\", " +
            "SUM(total_novedades)   AS \"totalNovedades\", " +

            "SUM(citas_completadas)::float / NULLIF(SUM(citas_asignadas),0)*100 AS \"cumplimientoCitasPromedio\"" +

            "FROM reportes_turno " +
            "WHERE EXTRACT(MONTH FROM fecha_turno)= $1 AND " +
            " EXTRACT(YEAR FROM fecha_turno)= $2 " +
            " AND id_regional = $3 " +
            "GROUP BY dia " +
            "ORDER BY dia;";

    public static final String  FIND_MOTIVOS_CANCELACION_ANUAL = "SELECT " +
            " EXTRACT (MONTH FROM public.registro_cancelacion_citas.fecha_cancelacion) AS mes, " +
            "  public.motivo_cancelacion.descripcion, " +
            "  COUNT(public.registro_cancelacion_citas.id_cita) AS \"cantidadCancelaciones\" " +
            "FROM public.registro_cancelacion_citas " +
            "INNER JOIN public.motivo_cancelacion ON " +
            "public.motivo_cancelacion.id = public.registro_cancelacion_citas.id_motivo_cancelacion " +
            "INNER JOIN public.citas ON " +
            "public.registro_cancelacion_citas.id_cita = public.citas.id_cita AND public.citas.id_regional = $1 " +
            "WHERE EXTRACT(YEAR FROM public.registro_cancelacion_citas.fecha_cancelacion)= $2 " +
            "GROUP BY public.motivo_cancelacion.descripcion,public.registro_cancelacion_citas.fecha_cancelacion " +
            "ORDER BY public.registro_cancelacion_citas.fecha_cancelacion;";

    public static final String  FIND_MOTIVOS_CANCELACION_MENSUAL = "SELECT " +
            "   EXTRACT (DAY FROM public.registro_cancelacion_citas.fecha_cancelacion) AS dia, " +
            "    public.motivo_cancelacion.descripcion, " +
            "    COUNT(public.registro_cancelacion_citas.id_cita) AS \"cantidadCancelaciones\" " +
            "FROM public.registro_cancelacion_citas " +
            "INNER JOIN public.motivo_cancelacion ON " +
            "public.motivo_cancelacion.id = public.registro_cancelacion_citas.id_motivo_cancelacion " +
            "INNER JOIN public.citas ON " +
            "public.registro_cancelacion_citas.id_cita = public.citas.id_cita AND public.citas.id_regional = $1 " +
            "WHERE   EXTRACT(MONTH FROM public.registro_cancelacion_citas.fecha_cancelacion)= $2 AND " +
            "   EXTRACT(YEAR FROM public.registro_cancelacion_citas.fecha_cancelacion)= $3 " +
            "GROUP BY public.motivo_cancelacion.descripcion,public.registro_cancelacion_citas.fecha_cancelacion " +
            "ORDER BY public.registro_cancelacion_citas.fecha_cancelacion;";

    private  QueryReportes() {
        throw new IllegalStateException("Utility class");
    }

}
