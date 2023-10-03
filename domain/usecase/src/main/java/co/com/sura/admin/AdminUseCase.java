package co.com.sura.admin;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.agenda.Conductor;
import co.com.sura.entity.agenda.Movil;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.entity.admin.*;
import co.com.sura.entity.agenda.Profesional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.List;

public class AdminUseCase implements RemisionFactory {

   private final RemisionCrudRepository remisionCrudRepository;

    public AdminUseCase(RemisionCrudRepository remisionRepository) {
        this.remisionCrudRepository = remisionRepository;
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
        return remisionCrudRepository.consultarHistoricoRemision(idRemision);
    }

    public Mono<Boolean> actualizarRemisionPorNovedad(
            RemisionRequest remisionRequest, List<CitaRequest> citas, NovedadRequest novedadRequest){
        return remisionCrudRepository.actualizarRemisionPorNovedad(remisionRequest,citas,novedadRequest);
    }

    public Mono<RegistroHistorialRemision> consultarDataActualRemision(String idRemision){
        return remisionCrudRepository.consultarDatosRemision(idRemision);
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



    //farmacia
    public Flux<PacienteTratamientoCita> consultarAllTratamientosToFarmacia(){
        return remisionCrudRepository.consultarAllPacienteWithMedicamentosToFarmacia();
    }
    public Flux<PacienteTratamientoCita> consultarAllTratamientosToFarmaciaWithFilter(
            LocalDate fechaTurno, Integer idHorario, String idRegional){
       return remisionCrudRepository.consultarAllPacienteWithMedicamentosToFarmaciaByFilter(
               fechaTurno,idHorario,idRegional);
    }
    public Mono<Boolean> notificarMedicamentosToFarmacia (List<PacienteTratamientoCita> tratamientoCitasList){
        return remisionCrudRepository.notificarMedicamentosToFarmacia(tratamientoCitasList);
    }


    //profesionales
    public Flux<Profesional> consultarProfesional() {
        return remisionCrudRepository.consultarProfesionales();
    }
    public Mono<Profesional> crearProfesional(Profesional profesional) {
        return remisionCrudRepository.crearProfesional(profesional);
    }
    public Mono<Profesional> actualizarProfesional(Profesional profesional) {
        return remisionCrudRepository.actualizarProfesional(profesional);
    }

    public  Flux<Conductor> consultarConductores(){
        return remisionCrudRepository.consultarConductores();
    }

    //conductor
    public Mono<Conductor> crearConductor(Conductor conductor){return remisionCrudRepository.crearConductor(conductor);}
    public Mono<Conductor> actualizarConductor(Conductor conductor){
        return remisionCrudRepository.actualizarConductor(conductor);}

    //moviles
   public Mono<Movil> crearMovil(Movil movil){return remisionCrudRepository.crearMovil(movil);}

    public Mono<Movil> actualizarMovil(Movil movil){
          return remisionCrudRepository.actualizarMovil(movil);}

    public Flux<Movil> consultarMoviles(){
        return remisionCrudRepository.consultarMoviles();
    }
    public Flux<Movil> consultarMovilesSinConductor(){
        return remisionCrudRepository.consultarMovilesSinConductor();
    }
    public Flux<Movil> consultarMovilesByIdRegional(String idRegional){
        return remisionCrudRepository.consultarMovilesByIdRegional(idRegional);
    }

}
