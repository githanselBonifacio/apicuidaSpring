package co.com.sura.remision;

import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.NovedadRequest;
import co.com.sura.remision.dto.RemisionRequest;
import co.com.sura.remision.gateway.HistorialRemisionRepository;
import co.com.sura.remision.entity.datosremision.DatosAtencionPaciente;
import co.com.sura.remision.entity.datosremision.Paciente;
import co.com.sura.remision.entity.historial.RegistroHistorialRemision;
import co.com.sura.remision.entity.Remision;
import co.com.sura.remision.gateway.RemisionCrudRepository;
import co.com.sura.remision.factory.RemisionFactory;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

public class RemisionUseCase implements RemisionFactory {

   private final RemisionCrudRepository remisionCrudRepository;
   private final HistorialRemisionRepository historialRemisionRepository;


    public RemisionUseCase(RemisionCrudRepository remisionRepository,
                           HistorialRemisionRepository historialRemisionRepository) {
        this.remisionCrudRepository = remisionRepository;
        this.historialRemisionRepository = historialRemisionRepository;

    }

    //remisiones
    public Flux<Remision> consultarRemisiones(){
        return remisionCrudRepository.consultarRemisiones();
    }

    public Mono<Boolean> crearRemisionCitas (RemisionRequest remisionRequest, List<CitaRequest> citas){
        return remisionCrudRepository
                .crearRemisionCita(
                        remisionRequest,
                        citas
                );
    }
    public Flux<RegistroHistorialRemision> consultarHistorialRemisionById (String idRemision){
        return historialRemisionRepository.consultarHistoricoRemision(idRemision);
    }

    public Mono<Boolean> actualizarRemisionPorNovedad(
            RemisionRequest remisionRequest, List<CitaRequest> citas, NovedadRequest novedadRequest){
        return remisionCrudRepository.actualizarRemisionPorNovedad(remisionRequest,citas,novedadRequest);
    }

    public Mono<RegistroHistorialRemision> consultarDataActualRemision(String idRemision){
        return historialRemisionRepository.consultarDatosRemision(idRemision);
    }

    public Mono<Boolean> egresarRemisionById(String idRemision){
        return remisionCrudRepository.egresarRemisionById(idRemision);
    }
    //datos paciente
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByRemision(String idRemision){
        return remisionCrudRepository.consultarDatosAtencionPacienteByIdRemision(idRemision);
    }

    public Mono<Paciente> consultarPacienteFromRemision(String idRemision){
        return remisionCrudRepository.consultarPacienteFromRemision(idRemision);
    }

}
