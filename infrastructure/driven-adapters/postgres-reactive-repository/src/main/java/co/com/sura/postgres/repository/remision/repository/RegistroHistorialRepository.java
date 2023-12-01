package co.com.sura.postgres.repository.remision.repository;

import co.com.sura.postgres.repository.remision.data.RegistroHistorialRemisionData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;

public interface RegistroHistorialRepository extends ReactiveCrudRepository<RegistroHistorialRemisionData,Integer> {

    @Query("SELECT COUNT(*) " +
            "FROM public.historial_remision " +
            "INNER JOIN public.remisiones ON public.remisiones.id_remision = public.historial_remision.id_remision " +
            "AND public.remisiones.id_regional = $2 " +
            "WHERE fecha_registro::date = $1")
    Mono<Integer> countByFechaNovedadRegional(LocalDate fechaRegistro, String idRegional);

    @Query("select * from  public.historial_remision WHERE id_remision = $1 ORDER BY fecha_registro DESC;")
    Flux<RegistroHistorialRemisionData> findAllByIdRemision(String idRemision);

    @Query("SELECT public.remisiones.*, " +
            "  row_to_json(pacientes) as paciente, " +
            "  row_to_json(ubicaciones) as ubicacion_paciente, " +
            "  row_to_json(datos_atencion_paciente) as datos_atencion, " +
            "  (SELECT json_agg(d) FROM " +
            "  (SELECT * FROM remision_diagnostico WHERE id_remision = $1) d) as diagnosticos, " +
            "  (SELECT json_agg(json_build_object(" +
            "   'datos_cita',  c, " +
            "   'tratamientos',(SELECT json_agg(t) FROM tratamientos t WHERE t.id_cita = c.id_cita),"+
            "   'procedimientos', json_build_object(" +
            "   'canalizaciones', (SELECT json_agg(ca) FROM canalizaciones ca WHERE ca.id_cita = c.id_cita), " +
            "   'curaciones', (SELECT json_agg(cu) FROM curaciones cu WHERE cu.id_cita = c.id_cita), " +
            "   'fototerapias', (SELECT json_agg(fo) FROM fototerapias fo WHERE fo.id_cita = c.id_cita), " +
            "   'secreciones', (SELECT json_agg(se) FROM secreciones se WHERE se.id_cita = c.id_cita), " +
            "   'sondajes', (SELECT json_agg(son) FROM sondajes son WHERE son.id_cita = c.id_cita), " +
            "   'soporte_nutricionales', " +
            "            (SELECT json_agg(sopor) FROM soporte_nutricionales sopor WHERE sopor.id_cita = c.id_cita), " +
            "   'toma_muestra', (SELECT json_agg(to_mu) FROM toma_muestras to_mu WHERE to_mu.id_cita = c.id_cita) " +
            "    )))" +
            "  FROM (SELECT citas.* FROM citas " +
            "         WHERE id_remision = $1 AND (id_estado = 1 OR id_estado = 2) AND fecha_programada > $2 " +
            "        ORDER BY fecha_programada ASC) c )as citas " +
            "   FROM public.remisiones " +
            "   INNER JOIN  public.pacientes ON public.pacientes.numero_identificacion " +
            "      = public.remisiones.numero_identificacion_paciente " +
            "   INNER JOIN  public.datos_atencion_paciente ON public.datos_atencion_paciente.id_remision " +
            "        = public.remisiones.id_remision " +
            "   INNER JOIN  public.ubicaciones ON public.ubicaciones.id_ubicacion =   public.pacientes.id_ubicacion " +
            "   WHERE public.remisiones.id_remision = $1;")
    Mono<RegistroHistorialRemisionData> buildByIdRemisionForUpdate
            (String idRemision, LocalDateTime fechaAplicacionNovedad);

    @Query("SELECT public.remisiones.*, " +
            "  row_to_json(pacientes) as paciente, " +
            "  row_to_json(ubicaciones) as ubicacion_paciente, " +
            "  row_to_json(datos_atencion_paciente) as datos_atencion, " +
            "  (SELECT json_agg(d) FROM " +
            "  (SELECT * FROM remision_diagnostico WHERE id_remision = $1) d) as diagnosticos, " +
            "  (SELECT json_agg(json_build_object(" +
            "   'datos_cita',  c, " +
            "   'tratamientos',(SELECT json_agg(t) FROM tratamientos t WHERE t.id_cita = c.id_cita),"+
            "   'procedimientos', json_build_object(" +
            "   'canalizaciones', (SELECT json_agg(ca) FROM canalizaciones ca WHERE ca.id_cita = c.id_cita), " +
            "   'curaciones', (SELECT json_agg(cu) FROM curaciones cu WHERE cu.id_cita = c.id_cita), " +
            "   'fototerapias', (SELECT json_agg(fo) FROM fototerapias fo WHERE fo.id_cita = c.id_cita), " +
            "   'secreciones', (SELECT json_agg(se) FROM secreciones se WHERE se.id_cita = c.id_cita), " +
            "   'sondajes', (SELECT json_agg(son) FROM sondajes son WHERE son.id_cita = c.id_cita), " +
            "   'soporte_nutricionales', " +
            "          (SELECT json_agg(sopor) FROM soporte_nutricionales sopor WHERE sopor.id_cita = c.id_cita), " +
            "   'toma_muestra', (SELECT json_agg(to_mu) FROM toma_muestras to_mu WHERE to_mu.id_cita = c.id_cita) " +
            "    )))" +
            "  FROM (SELECT citas.* FROM citas " +
            "         WHERE id_remision = $1 " +
            "        ORDER BY fecha_programada ASC) c )as citas " +
            "   FROM public.remisiones " +
            "   INNER JOIN  public.pacientes ON public.pacientes.numero_identificacion " +
            "      = public.remisiones.numero_identificacion_paciente " +
            "   INNER JOIN  public.datos_atencion_paciente ON public.datos_atencion_paciente.id_remision " +
            "        = public.remisiones.id_remision " +
            "   INNER JOIN  public.ubicaciones ON public.ubicaciones.id_ubicacion =   public.pacientes.id_ubicacion " +
            "   WHERE public.remisiones.id_remision = $1;")
    Mono<RegistroHistorialRemisionData> buildByIdRemisionActual
            (String idRemision);
}
