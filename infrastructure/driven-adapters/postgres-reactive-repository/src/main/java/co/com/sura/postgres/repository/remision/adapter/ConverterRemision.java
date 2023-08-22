package co.com.sura.postgres.repository.remision.adapter;


import co.com.sura.dto.remision.*;
import co.com.sura.entity.remision.*;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.remision.data.*;
import com.fasterxml.jackson.databind.util.JSONPObject;
import com.google.gson.Gson;
import com.google.gson.JsonObject;
import io.r2dbc.postgresql.codec.Json;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;
import reactor.util.function.Tuple2;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class ConverterRemision {
    protected static Object convertToJsonObject (Json jsonByteArrayInput ){
        var gson = new Gson();
        byte[] byteArray = jsonByteArrayInput.asArray();
        var jsonString = new String(byteArray);

        return   gson.fromJson(jsonString, Object.class);
    }
    public static RemisionData convertToRemisionRequest(RemisionRequest remisionRequest){

        return new RemisionData()
                .toBuilder()
                .idRemision(remisionRequest.getIdRemision())
                .institucionRemite(remisionRequest.getInstitucionRemite())
                .programa(remisionRequest.getPrograma().getNombre())
                .fechaAdmision(remisionRequest.getFechaAdmision())
                .tipoAdmision(remisionRequest.getTipoAdmision())
                .numeroIdentificacionPaciente(remisionRequest.getNumeroIdentificacion())
                .estado(remisionRequest.getEstado())
                .build();
    }
    public static  Paciente convertToPaciente (PacienteData pacienteData){
        return new Paciente()
                .toBuilder()
                .tipoIdentificacion(pacienteData.getTipoIdentificacion())
                .numeroIdentificacion(pacienteData.getNumeroIdentificacion() )
                .nombre(pacienteData.getNombre())
                .apellido(pacienteData.getApellido())
                .sexo(pacienteData.getSexo())
                .peso(pacienteData.getPeso())
                .edad(pacienteData.getEdad())
                .tipoAfiliacion(pacienteData.getTipoAfiliacion())
                .nombreAseguradora(pacienteData.getNombreAseguradora())
                .fechaNacimiento(pacienteData.getFechaNacimiento())
                .build();
    }
    public static Ubicacion convertToUbicacion(UbicacionData ubicacionData){
        return  new Ubicacion()
                .toBuilder()
                .latitud(ubicacionData.getLatitud())
                .longitud(ubicacionData.getLongitud())
                .direccion(ubicacionData.getDireccion())
                .tipoVia(ubicacionData.getTipoVia())
                .numero1(ubicacionData.getNumero1())
                .numeroInterseccion(ubicacionData.getNumeroInterseccion())
                .numero2(ubicacionData.getNumero2())
                .barrio(ubicacionData.getBarrio())
                .sinNomenclatura(ubicacionData.getSinNomenclatura())
                .municipio(ubicacionData.getMunicipio())
                .build();
    }
    public static UbicacionData extraerUbicacionData(RemisionRequest remisionRequest){
        UbicacionRequest ubicacionRequest = remisionRequest.getDatosAtencionPaciente().getUbicacion();
        return new UbicacionData()
                .toBuilder()
                .idUbicacion(remisionRequest.getIdRemision()+"_ubicacion")
                .latitud(ubicacionRequest.getLatitud())
                .longitud(ubicacionRequest.getLongitud())
                .direccion(ubicacionRequest.getDireccion())
                .tipoVia(ubicacionRequest.getTipoVia())
                .numero1(ubicacionRequest.getNumero1())
                .numeroInterseccion(ubicacionRequest.getNroInterseccion())
                .numero2(ubicacionRequest.getNumero2())
                .barrio(ubicacionRequest.getBarrio())
                .sinNomenclatura(ubicacionRequest.isSinNomenclatura())
                .municipio(ubicacionRequest.getMunicipio().getNombre())
                .idCiudad(ubicacionRequest.getMunicipio().getIdCiudad())
                .build();
    }

    public static PacienteData extraerPacienteData(RemisionRequest remisionRequest){
        return new PacienteData()
                .toBuilder()
                .tipoIdentificacion(remisionRequest.getTipoIdentificacion().getNombre())
                .numeroIdentificacion(remisionRequest.getNumeroIdentificacion())
                .nombre(remisionRequest.getNombre())
                .apellido(remisionRequest.getApellido())
                .edad(remisionRequest.getEdad())
                .sexo(remisionRequest.getSexo())
                .peso(remisionRequest.getPeso())
                .tipoAfiliacion(remisionRequest.getTipoIdentificacion().getNombre())
                .nombreAseguradora(remisionRequest.getTipoAfiliacion().getNombreAseguradora())
                .fechaNacimiento(remisionRequest.getFechaNacimiento())
                .idUbicacion(remisionRequest.getIdRemision()+"_ubicacion")
                .build();
    }
    public static List<RemisionDiagnosticoData> extraerRemisionDiagnosticoData(
            List<Diagnostico> diagnosticos, String idRemision){

        return diagnosticos
                .stream()
                .map(diagnostico -> {
                    return new RemisionDiagnosticoData()
                            .toBuilder()
                            .idRemision(idRemision)
                            .nombreDiagnostico(diagnostico.getNombre())
                            .build();
                })
                .collect(Collectors.toList());
    }

    public static CitaData convertirCitaData(CitaRequest citaRequest){
        return new CitaData()
                .toBuilder()
                .idCita(citaRequest.getIdCita())
                .duracion(citaRequest.getDuracion())
                .holgura(citaRequest.getHolgura())
                .fechaInicio(citaRequest.getFechaInicio())
                .especialidad(citaRequest.getEspecialidad())
                .idCita(citaRequest.getIdCita())
                .build();
    }

    public static List<CitaData> convertirCitasDataList(
            List<CitaRequest> citasRequest, RemisionRequest remisionRequest){

        return citasRequest
                .stream()
                .map(ConverterRemision :: convertirCitaData)
                .peek(citaData -> citaData.setIdRemision(remisionRequest.getIdRemision()))
                .peek(citaData -> citaData.setIdCiudad(remisionRequest.getCiudad().getIdCiudad()))
                .peek(citaData -> citaData.setLatitud(remisionRequest
                        .getDatosAtencionPaciente().getUbicacion().getLatitud()))
                .peek(citaData -> citaData.setLongitud(remisionRequest
                        .getDatosAtencionPaciente().getUbicacion().getLongitud()))
                .collect(Collectors.toList());
    }
    public static DatosAtencionPacienteData convertirDatosAtencionPacienteData(
            DatosAtencionPacienteRequest datosAtencionPacienteRequest, String idRemision){
        return new DatosAtencionPacienteData()
                .toBuilder()
                .nombreCuidador(datosAtencionPacienteRequest.getNombreCuidador())
                .nombreResponsable(datosAtencionPacienteRequest.getNombreResponsable())
                .telefonoPaciente(datosAtencionPacienteRequest.getTelefonoPaciente())
                .celularPaciente(datosAtencionPacienteRequest.getCelularPaciente())
                .celularPaciente2(datosAtencionPacienteRequest.getCelularPaciente2())
                .idRemision(idRemision)
                .build();
    }
    protected static  TratamientoData convetirTratamientoData (TratamientoRequest tratamientoRequest){
        return new TratamientoData()
                .toBuilder()
                .tipoTratamiento(tratamientoRequest.getTipoTratamiento().getNombre())
                .idMedicamento(tratamientoRequest.getMedicamento().getIdMedicamento())
                .nombreMedicamento(tratamientoRequest.getMedicamento().getNombre())
                .codigoMedicamento(tratamientoRequest.getMedicamento().getCodigoMedicamento())
                .presentacionMedicamento(tratamientoRequest.getMedicamento().getPresentacion())
                .cantidadDosis((int) tratamientoRequest.getCantidadDosis())
                .unidadDosis(tratamientoRequest.getUnidadDosis().getDescripcion())
                .viaAdministracion(tratamientoRequest.getViaAdministracion().getDescripcion())
                .frecuencia(tratamientoRequest.getFrecuencia().getDescripcion())
                .duracion((int) tratamientoRequest.getDuracion())
                .noPBS(tratamientoRequest.getNoPBS())
                .tipoPrestacion(tratamientoRequest.getTipoPrestacion().getTipoPrestacion())
                .build();
    }
    public  static  List<TratamientoData> extraerTratamientoData(List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getTratamientos()
                        .stream()
                        .map(ConverterRemision ::convetirTratamientoData)
                        .peek(tratamientoData -> tratamientoData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static CanalizacionData convertirCanalizacionData(Canalizacion canalizacion ){
        return new CanalizacionData()
                .toBuilder()
                .tipoCanalizacion(canalizacion.getTipoCanalizacion())
                .tipoPrestacion(canalizacion.getTipoPrestacion())
                .build();
    }

    public  static  List<CanalizacionData> extraerCanalizacionData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getCanalizaciones()
                        .stream()
                        .map(ConverterRemision :: convertirCanalizacionData)
                        .peek(canalizacionData -> canalizacionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static FototerapiaData convertirFototerapiaData(Fototerapia fototerapia ){
        return new FototerapiaData()
                .toBuilder()
                .diasTratamiento(fototerapia.getDiasTratamiento())
                .cantidadDosis(fototerapia.getCantidadDosis())
                .tipofrecuencia(fototerapia.getTipoFrecuencia())
                .tipoPrestacion(fototerapia.getTipoPrestacion())
                .build();
    }

    public  static  List<FototerapiaData> extraerFototerapiaData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getFototerapias()
                        .stream()
                        .map(ConverterRemision :: convertirFototerapiaData)
                        .peek(fototerapiaData -> fototerapiaData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static SecrecionData convertirSecrecionData(SecrecionRequest secrecionRequest ){
        return new SecrecionData()
                .toBuilder()
                .diasTratamiento(Integer.parseInt(secrecionRequest.getDiasTratamiento()))
                .envioAspirador(secrecionRequest.isEnvioAspirador())
                .isNasal(secrecionRequest.isNasal())
                .isTraqueostomia(secrecionRequest.isTraqueostomia())
                .visitaEnfermeria(secrecionRequest.isVisitaEnfermeria())
                .tipoSonda(secrecionRequest.getTipoSonda())
                .tipoPrestacion(secrecionRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<SecrecionData> extraerSecrecionData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSecreciones()
                        .stream()
                        .map(ConverterRemision :: convertirSecrecionData)
                        .peek(secrecionData -> secrecionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static SondajeData convertirSondajeData(SondajeRequest sondajeRequest ){
        return new SondajeData()
                .toBuilder()
                .sondaje(sondajeRequest.getSondaje())
                .tipoSondaje(sondajeRequest.getTipoSondaje())
                .totalSesiones(sondajeRequest.getTotalSesiones())
                .tipoPrestacion(sondajeRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<SondajeData> extraerSondajeData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSondajes()
                        .stream()
                        .map(ConverterRemision :: convertirSondajeData)
                        .peek(sondajeData -> sondajeData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }
    protected static SoporteNutricionalData convertirSoporteNutricional(
            SoporteNutricionalRequest soporteNutricionalRequest ){
        return new SoporteNutricionalData()
                .toBuilder()
                .descripcion(soporteNutricionalRequest.getDescripcion())
                .tipo(soporteNutricionalRequest.getTipo())
                .unidadDosis(soporteNutricionalRequest.getUnidadDosis().getDescripcion())
                .duracion(soporteNutricionalRequest.getDuracion())
                .volumen(soporteNutricionalRequest.getVolumen())
                .idMedicamento(soporteNutricionalRequest.getMedicamento().getIdMedicamento())
                .nombreMedicamento(soporteNutricionalRequest.getMedicamento().getNombre())
                .codigoMedicamento(soporteNutricionalRequest.getMedicamento().getCodigoMedicamento())
                .presentacionMedicamento(soporteNutricionalRequest.getMedicamento().getPresentacion())
                .cantidadDosis(soporteNutricionalRequest.getCantidadDosis())
                .noPBS(soporteNutricionalRequest.isNoPBS())
                .tipoPrestacion(soporteNutricionalRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<SoporteNutricionalData> extraerSoporteNutricionalData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSoporteNutricionales()
                        .stream()
                        .map(ConverterRemision :: convertirSoporteNutricional)
                        .peek(soporteNutricionalData -> soporteNutricionalData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }
    protected static TomaMuestraData convertirSoporteNutricional(TomaMuestraRequest tomaMuestraRequest ){
        return new TomaMuestraData()
                .toBuilder()
                .tipoMuestra(tomaMuestraRequest.getTipoMuestra().getDescripcion())
                .requiereAyuno(tomaMuestraRequest.isRequiereAyuno())
                .tipoPrestacion(tomaMuestraRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<TomaMuestraData> extraerSoporteTomaMuestraData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getTomaMuestras()
                        .stream()
                        .map(ConverterRemision :: convertirSoporteNutricional)
                        .peek(tomaMuestraData ->  tomaMuestraData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static CuracionData convertirCuracionData(CuracionRequest curacionRequest ){
        return new CuracionData()
                .toBuilder()
                .tipoCuracion(curacionRequest.getTipoCuracion().getDescripcion())
                .descripcion(curacionRequest.getDescripcion())
                .sesiones(curacionRequest.getSesiones())
                .build();
    }

    public  static  List<CuracionData> extraerCuracionData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getCuraciones()
                        .stream()
                        .map(ConverterRemision :: convertirCuracionData)
                        .peek(curacionData ->  curacionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    public static DatosAtencionPaciente convertToDatosAtencionPaciente(
            DatosAtencionPacienteData datosAtencionPacienteData){
        return new DatosAtencionPaciente()
                .toBuilder()
                .nombreCuidador(datosAtencionPacienteData.getNombreCuidador())
                .nombreResponsable(datosAtencionPacienteData.getNombreResponsable())
                .telefonoPaciente(datosAtencionPacienteData.getTelefonoPaciente())
                .celularPaciente2(datosAtencionPacienteData.getCelularPaciente2())
                .celularPaciente(datosAtencionPacienteData.getCelularPaciente())
                .build();
    }

    public static  RegistroHistorialRemision convertToRegistroHistoriaRemision(
            RegistroHistorialRemisionData registroHistorialRemisionData){
        var builder = new RegistroHistorialRemision().toBuilder()
                .idRemision(registroHistorialRemisionData.getIdRemision())
                .fechaAplicacionNovedad(registroHistorialRemisionData.getFechaAplicacionNovedad())
                .motivoNovedad(registroHistorialRemisionData.getMotivoNovedad())
                .estado(registroHistorialRemisionData.getEstado())
                .fechaAdmision(registroHistorialRemisionData.getFechaAdmision())
                .programa(registroHistorialRemisionData.getPrograma())
                .tipoAdmision(registroHistorialRemisionData.getTipoAdmision())
                .institucionRemite(registroHistorialRemisionData.getInstitucionRemite())
                .paciente(convertToJsonObject(registroHistorialRemisionData.getPaciente()))
                .datosAtencion(convertToJsonObject(registroHistorialRemisionData.getDatosAtencion()))
                .ubicacionPaciente(convertToJsonObject(registroHistorialRemisionData.getUbicacionPaciente()))
                .diagnosticos(convertToJsonObject(registroHistorialRemisionData.getDiagnosticos()));

        if (registroHistorialRemisionData.getCitas() != null) {
            builder.citas(convertToJsonObject(registroHistorialRemisionData.getCitas()));
        }
        return builder.build();
    }
}
