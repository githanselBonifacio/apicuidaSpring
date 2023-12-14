package co.com.sura.remision.gateway;

import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.NovedadRequest;
import co.com.sura.remision.dto.RemisionRequest;
import co.com.sura.remision.entity.Remision;
import co.com.sura.remision.entity.datosremision.DatosAtencionPaciente;
import co.com.sura.remision.entity.datosremision.Paciente;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.List;

public interface RemisionCrudRepository {
    //remisiones
    Flux<Remision>                   consultarRemisiones();
    Mono<Boolean>                    crearRemisionCita (RemisionRequest remisionRequest, List<CitaRequest> citas);

    Mono<Boolean>                    actualizarRemisionPorNovedad(
                                                    RemisionRequest remisionRequest,
                                                    List<CitaRequest> citas, NovedadRequest novedadRequest);

    Mono<Boolean>                    egresarRemisionById(String idRemision);

    //datos paciente
    Mono<DatosAtencionPaciente>      consultarDatosAtencionPacienteByIdRemision (String idRemision);
    Mono<Paciente>                   consultarPacienteFromRemision (String idRemision);



}
