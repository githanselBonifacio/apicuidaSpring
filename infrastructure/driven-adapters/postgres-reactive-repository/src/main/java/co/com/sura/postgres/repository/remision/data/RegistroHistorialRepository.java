package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDateTime;

public interface RegistroHistorialRepository extends ReactiveCrudRepository<RegistroHistorialRemisionData,Integer> {

    @Query("select * from  public.historial_remision WHERE id_remision = $1;")
    Flux<RegistroHistorialRemisionData> findAllByIdRemision(String idRemision);


    @Query("SELECT public.remision.*, " +
            "  row_to_json(paciente) as paciente, " +
            "  row_to_json(ubicacion) as ubicacion_paciente, " +
            "  row_to_json(datos_atencion_paciente) as datos_atencion, " +
            "  (SELECT json_agg(d) FROM " +
            "  (SELECT * FROM remision_diagnostico WHERE id_remision = $1) d) as diagnosticos, " +
            "  (SELECT json_agg(json_build_object(" +
            "   'datos_cita',  c, " +
            "   'tratamientos',(SELECT json_agg(t) FROM tratamiento t WHERE t.id_cita = c.id_cita),"+
            "   'procedimientos', json_build_object(" +
            "   'canalizaciones', (SELECT json_agg(ca) FROM canalizacion ca WHERE ca.id_cita = c.id_cita), " +
            "   'curaciones', (SELECT json_agg(cu) FROM curacion cu WHERE cu.id_cita = c.id_cita), " +
            "   'fototerapias', (SELECT json_agg(fo) FROM fototerapia fo WHERE fo.id_cita = c.id_cita), " +
            "   'secreciones', (SELECT json_agg(se) FROM secrecion se WHERE se.id_cita = c.id_cita), " +
            "   'sondajes', (SELECT json_agg(son) FROM sondaje son WHERE son.id_cita = c.id_cita), " +
            "   'soporte nutricionales', " +
            "               (SELECT json_agg(sopor) FROM soporte_nutricional sopor WHERE sopor.id_cita = c.id_cita), " +
            "   'toma de muestra', (SELECT json_agg(to_mu) FROM toma_muestra to_mu WHERE to_mu.id_cita = c.id_cita) " +
            "    )))" +
            "  FROM (SELECT cita.* FROM cita " +
            "         WHERE id_remision = $1 AND (id_estado = 1 OR id_estado = 2) AND fecha_programada > $2 " +
            "        ) c )as citas " +
            "   FROM public.remision " +
            "   INNER JOIN  public.paciente ON public.paciente.numero_identificacion " +
            "      = public.remision.numero_identificacion_paciente " +
            "   INNER JOIN  public.datos_atencion_paciente ON public.datos_atencion_paciente.id_remision " +
            "        = public.remision.id_remision " +
            "   INNER JOIN  public.ubicacion ON public.ubicacion.id_ubicacion =   public.paciente.id_ubicacion " +
            "   WHERE public.remision.id_remision = $1;")
    Mono<RegistroHistorialRemisionData> buildByIdRemision(String idRemision, LocalDateTime fechaAplicacionNovedad);
}
