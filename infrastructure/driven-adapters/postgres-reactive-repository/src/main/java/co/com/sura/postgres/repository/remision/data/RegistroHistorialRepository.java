package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface RegistroHistorialRepository extends ReactiveCrudRepository<RegistroHistorialRemisionData,Integer> {

    @Query("select * from  public.historial_remision WHERE id_remision = $1;")
    Flux<RegistroHistorialRemisionData> findAllByIdRemision(String idRemision);

    @Query("SELECT public.remision.*, " +
            "row_to_json(paciente) as paciente, " +
            "row_to_json(ubicacion) as ubicacion_paciente, " +
            "row_to_json(datos_atencion_paciente) as datos_atencion, " +
            "(SELECT json_agg(d) FROM " +
            "(SELECT * FROM remision_diagnostico WHERE id_remision = $1) d) as diagnosticos, " +
            "(SELECT json_agg(c) FROM " +
            "(SELECT * FROM cita WHERE id_remision = $1) c) as citas " +
            "FROM public.remision " +
            "INNER JOIN  public.paciente ON public.paciente.numero_identificacion " +
            "= public.remision.numero_identificacion_paciente " +
            "INNER JOIN  public.datos_atencion_paciente ON public.datos_atencion_paciente.id_remision " +
            "= public.remision.id_remision " +
            "INNER JOIN  public.ubicacion ON public.ubicacion.id_ubicacion =   public.paciente.id_ubicacion " +
            "WHERE public.remision.id_remision = $1;")
    Mono<RegistroHistorialRemisionData> buildByIdRemision(String idRemision);
}
